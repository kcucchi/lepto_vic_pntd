
#' ---
#' title: "Regression of lepto counts wrt risk factors at the weekly-county level"
#' author: "Karina Cucchi"
#' date: "November 14th, 2017"
#' ---

library(splines)
library(pbs)
library(dlnm)
library(ggplot2)
library(MASS)
library(dplyr)

library(sandwich)
library(lmtest)

library(car) # for VIF

# contains code for the definition of constrained splines 
# for time spline each year
source('../../../../R_utils/cobs_perYear.R')

#'
#' # Get and format data #
#'

#'
#' ## get data ##
#'

# get data at weekly and county resolution
df_cnt_week <- 
  readRDS('../../../data_lepto/2_formatted_data/df_cnt_week_final.rds')

# limit to years 2004-2014
# (years of lepto surveillance)

df_cnt_week <- 
  subset(x = df_cnt_week,y >= 2004)

head(df_cnt_week)

#'
#' ## refine by occupation : limit to farmers ##
#'

# df_cnt_week$count <- df_cnt_week$count_farmers
# head(df_cnt_week)

#'
#' ## refine in space ##
#'

df_cnt_week <- subset(df_cnt_week,cntWithLep)

# calculate the percentage of zeros
sum(df_cnt_week$count==0)/nrow(df_cnt_week)

#'
#' ## refine in time : limit to months of high peak ##
#'

# create subset variable without deleting main variable ##

# define indices to be applied to crossbases
row_keep <-
  which(df_cnt_week$m %in% 8:10)

# take subset for epi data
df_cnt_week_subset <- df_cnt_week[row_keep,]

# this is the percentage of zeros in the counts
sum(df_cnt_week_subset$count==0)/nrow(df_cnt_week_subset)


#'
#' ## change CNTY_CODE to factors ##
#'

# do it after removing certain counties to avoid having factors with zero counts
# (this messes up the cross-basis function)

df_cnt_week_subset$CNTY_CODE <- 
  as.factor(df_cnt_week_subset$CNTY_CODE)

# check how many counties
length(levels(df_cnt_week_subset$CNTY_CODE))




#'
#' Types of analysis: simple/multiple regression
#'

# prepare dataframe that will contain regression results
names_res <- c("var","regression_type","lag",
               "beta","se","beta_low","beta_high","p_value")
df_glm_res <- data.frame(matrix(NA,ncol=length(names_res),nrow=0))
names(df_glm_res) <- names_res;rm(names_res)

#'
#' Define vic variables of interest in analysis
#'

# names_vic contains the names of vic variables to include in the regressions
# suffix_pdf is the character to append at the end of pdf names

# names_vic <-
#   names(df_cnt_week)[grep(pattern = "^vic_",x = names(df_cnt_week))]
# suffix_pdf <- "all"
# names_vic = c('vic_Prcp','vic_roff','vic_sm1');suffix_pdf="hydro"
# names_vic = c('vic_Prcp','vic_roff','vic_sm1','vic_Tmin');suffix_pdf="Prcp,roff,sm1,Tmin"
# names_vic = c('vic_roff','vic_sm1');suffix_pdf="roff,sm1"
# names_vic = c('vic_roff','vic_sm1','vic_Tmin');suffix_pdf="roff,sm1,Tmin"
# names_vic = c('vic_Prcp','vic_sm1','vic_Tmin');suffix_pdf="Prcp,sm1,Tmin"
# -- same but without temperature as a control
names_vic = c('vic_Prcp','vic_sm1');suffix_pdf="Prcp,sm1"
# names_vic = c('vic_Prcp','vic_roff','vic_Tmin');suffix_pdf="Prcp,roff,Tmin"
# names_vic = c('vic_Prcp','vic_Tmin');suffix_pdf="Prcp,Tmin"
# -- same but without temperature as a control
# names_vic = c('vic_Prcp');suffix_pdf="Prcp"

#'
#' # Plot univariate regressions #
#'

list_plots_univ <- list()

i_vic = 1
for(i_vic in 1:length(names_vic)){
  
  list_plots_univ[[i_vic]] <-
    ggplot(data=subset(df_cnt_week,subset = count > 0),
           mapping = aes_string(x=names_vic[i_vic],y="log10(count)")) +
    geom_point() +
    geom_smooth(method="lm") +
    theme_bw()
  
}

# plot all in same figure
cowplot::plot_grid(plotlist = list_plots_univ)

# Doesn't make a lot of sense at the weekly level

#'
#' # Create basis vectors #
#'

#'
#' ## Long-term temporal trend ##
#'

# this is for the long-term temporal trend

bool_longTermTrend <- T

if(bool_longTermTrend){
  # # use cobs_perYear function to start and finsh at zeros every year
  ob.trend <- 
    cobs_perYear(df_week_year = df_cnt_week_subset,dfPerYear = 2)  
}

# 
# # check whether that's what we want
# # create dataframe for plotting
# df_plot_trend <- data.frame(as.data.frame(ob.trend),
#                             time=df_cnt_week_subset$time)
# # melt for ggplot
# df_plot_trend <-
#   reshape2::melt(df_plot_trend,id.vars="time")
# ggplot() +
#   geom_line(data = subset(df_plot_trend,
#                           variable %in% c("X2003_1","X2003_2","X2003_3",
#                                           "X2004_1","X2004_2","X2004_3")),
#             aes(x = time,y=value,colour=factor(variable))) +
#   xlim(0,200)
# # cool


#'
#' ## Seasonal component ##
#'

# ob.seasonal <- onebasis(x = df_cnt_week_subset$woy,
#                         fun = 'ns', #
#                         df=5)

#'
#' ## Environmental covariates ##
#'

# get rid of other vic variables in df_cnt_week
df_cnt_week <- 
  df_cnt_week[,setdiff(names(df_cnt_week),
                       setdiff(names(df_cnt_week)[grep(pattern = "^vic_",
                                                       x = names(df_cnt_week))],
                               names_vic))]


lag_max <- 15 # in weeks

i_vic <- 1
# create lagged variables
for(i_vic in 1:length(names_vic)){
  
  cb_iVic <-
    dlnm::crossbasis(x = df_cnt_week[,names_vic[i_vic]],
                     lag = lag_max,
                     argvar = list(fun="poly",deg=1),  
                     arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                     group = df_cnt_week$CNTY_CODE)
  
  # annoingly dlnm rescales values
  # define to original base
  df_cb_iVic <- as.data.frame(attributes(cb_iVic)$argvar$scale * cb_iVic)
  
  names(df_cb_iVic) <- 
    paste0(names_vic[i_vic],'_lag_',0:lag_max,'_weeks')
  
  # bind lagged values to df_cnt_week
  df_cnt_week <- 
    cbind(df_cnt_week,df_cb_iVic)
  
  # check that the lag and rescaling worked well
  # check the difference between original vector and calcuated lagged vector
  # allowing for small tolerance
  vect_orig <- df_cnt_week[,names_vic[i_vic]]
  vect_lag <- df_cnt_week[,paste0(names_vic[i_vic],'_lag_0_weeks')]
  vect_orig <- vect_orig[which(!is.na(vect_lag))]
  vect_lag <- vect_lag[which(!is.na(vect_lag))]
  if(!all(abs(vect_lag - vect_orig) < 1e-5)){
    stop(paste0('problem when calculating lags for variable ',names_vic[i_vic]))
  }
  # (df_cnt_week[1500:1510,c('vic_sm1_lag0weeks','vic_sm1')])
  
  # get rid of lag 0
  df_cnt_week <- 
    df_cnt_week[,setdiff(names(df_cnt_week),names_vic[i_vic])]
  
}

# join to subset dataframe
df_cnt_week_subset <- 
  dplyr::left_join(
    x = df_cnt_week_subset,
    y = df_cnt_week[,c("sundays",
                       "CNTY_CODE",
                       names(df_cnt_week)[grep(pattern = 'lag',x = names(df_cnt_week))])])
# take out the original vic variable (calculated by dlnm)
# these are the names to get rid of
names_remove <-
  names(df_cnt_week_subset)[which(grepl(pattern = 'vic',
                                        x = names(df_cnt_week_subset)) & 
                                    !(grepl(pattern = 'lag',
                                            x = names(df_cnt_week_subset))))]
df_cnt_week_subset <- 
  df_cnt_week_subset[,setdiff(names(df_cnt_week_subset),names_remove)]

head(df_cnt_week_subset)

#'
#' plot scatterplots at a 2 weeks lag (time of exposure)
#'


list_plots_univ <- list()

i_vic = 1
for(i_vic in 1:length(names_vic)){
  
  list_plots_univ[[i_vic]] <-
    ggplot(data=subset(df_cnt_week,subset = count > 0),
           mapping = aes_string(x=paste0(names_vic[i_vic],'_lag_2_weeks'),
                                y="log10(count)")) +
    geom_point() +
    geom_smooth(method="lm") +
    theme_bw()
  
}

# plot all in same figure
cowplot::plot_grid(plotlist = list_plots_univ)


#'
#' # Regression analysis #
#'

#'
#' ## simple analysis (grouped by variable) ##
#'

#' # initialize dataframe for results of the simple regression analysis 
#' # with respect to the lagged weekly vic variables
#' names_simple <- c("var","lag","var_lag",
#'                   "beta","se","p_value","beta_low","beta_high")
#' df_glm_simple <- data.frame(matrix(data = NA,nrow = 0,ncol = length(names_simple)))
#' names(df_glm_simple) <- names_simple
#' 
#' i_vic <- 1
#' for(i_vic in 1:length(names_vic)){
#'   
#'   # names of columns containing lagged variables of names_vic[i_vic]
#'   names_vic_i <- 
#'     names(df_cnt_week_subset)[
#'       grep(pattern = names_vic[i_vic],
#'            x = names(df_cnt_week_subset))]
#'   
#'   # this is the formula for the univariate regression
#'   if(bool_longTermTrend){
#'     formula_vic_i <- 
#'       paste0("count ~ ",
#'              paste0(names_vic_i,collapse = ' + '), 
#'              " + CNTY_CODE + ob.trend + offset(log(pop_agr))")  
#'   }else{
#'     formula_vic_i <- 
#'       paste0("count ~ ",
#'              paste0(names_vic_i,collapse = ' + '), 
#'              " + CNTY_CODE + offset(log(pop_agr))") 
#'   }
#'   
#'   
#'   # call glm
#'   glm_simple_i <- 
#'     stats::glm(formula = 
#'                  as.formula(formula_vic_i),
#'                family = quasipoisson(link='log'), 
#'                data = df_cnt_week_subset)
#'   
#'   # compute pvalues and SE with lmtest::coeftest
#'   ## Poisson model with SE estimated via robust variance estimator
#'   # z test
#'   glm_coeftest_simple_i <- 
#'     lmtest::coeftest(glm_simple_i,vcov=sandwich)
#'   
#'   # extract regression coefficient, se and p-value
#'   df_glm_simple_i<- 
#'     data.frame(var_lag = row.names(glm_coeftest_simple_i[names_vic_i,]),
#'                beta = as.numeric(glm_coeftest_simple_i[names_vic_i,'Estimate']),
#'                se = as.numeric(glm_coeftest_simple_i[names_vic_i,'Std. Error']),
#'                p_value = as.numeric(glm_coeftest_simple_i[names_vic_i,'Pr(>|z|)']),
#'                row.names = names_vic_i,
#'                stringsAsFactors = F)
#'   
#'   # define name and lag
#'   df_glm_simple_i$var <- names_vic[i_vic]
#'   df_glm_simple_i$lag <- 
#'     as.numeric(unlist(lapply(X = strsplit(x = df_glm_simple_i$var_lag,split = '_'),
#'                              FUN = function(l) l[[4]])))
#'   
#'   # calculate confidence intervals
#'   # takes about 2min
#'   test <- proc.time()
#'   conf_int <- 
#'     confint(object = glm_simple_i, 
#'             parm = names_vic_i, level = 0.95)
#'   proc.time() - test
#'   
#'   df_glm_simple_i[names_vic_i,'beta_low'] <- 
#'     unname(conf_int[,'2.5 %'])
#'   df_glm_simple_i[names_vic_i,'beta_high'] <- 
#'     unname(conf_int[,'97.5 %'])
#'   
#'   # append to the main dataframe
#'   df_glm_simple <- 
#'     rbind(df_glm_simple,
#'           df_glm_simple_i[,names(df_glm_simple)])
#'   
#'   # remove temporary variables
#'   rm(df_glm_simple_i,glm_coeftest_simple_i)
#'   
#' }
#' 
#' print(df_glm_simple)
#' 
#' #'
#' #' ## plot ##
#' #'  
#' 
#' # plot beta values for now, rate ratios will be plot later
#' 
#' ggplot(data = df_glm_simple,aes(x=as.numeric(lag),y=beta)) +
#'   geom_point() +
#'   geom_errorbar(mapping = aes(ymin=beta_low,ymax=beta_high)) +
#'   geom_hline(mapping = aes(yintercept=0),linetype="dashed") +
#'   labs(x='lag (in weeks)',y = expression(beta)) +
#'   facet_wrap( ~ var,nrow = 3) +
#'   theme_bw() +
#'   theme(legend.position = "top")
#' 
#' #'
#' #' replace in main dataframe
#' #'
#' 
#' df_glm_simple$regression_type <- "simple"
#' 
#' df_glm_res <- rbind(df_glm_res,df_glm_simple[,names(df_glm_res)])
#' 
#' rm(df_glm_simple)



#'
#' ## Now do multiple analysis ##
#'

# names of columns containing lagged variables of names_vic
names_vic_all <-
  names(df_cnt_week_subset)[grep(pattern='vic_',x=names(df_cnt_week_subset))]

#'
#' ### check correlation between variables ###
#'

# compute correlation matrix
df_cnt_week_forCor <-
  df_cnt_week %>%
  dplyr::select(names_vic_all) %>%
  subset(complete.cases(df_cnt_week))
mat_cor <- cor(df_cnt_week_forCor)

df_cor <- as.data.frame(mat_cor)
df_cor$Var1 <- row.names(df_cor)

df_cor_long <- tidyr::gather(data=df_cor,key='Var2',value='cor',-Var1)

str(df_cor_long)

ggplot() +
  geom_tile(data = df_cor_long,
            mapping = aes(x = factor(x = Var1,levels = names_vic_all),
                          y = factor(x = Var2,levels = names_vic_all),
                          fill=cor)) +
  geom_point(data = subset(df_cor_long,subset = abs(cor) >= 0.90),
             mapping = aes(x = Var1,y = Var2)) +
  geom_text(data = df_cor_long,
            mapping = aes(x = factor(x = Var1,levels = names_vic_all),
                          y = factor(x = Var2,levels = names_vic_all),
                          label=round(cor,1)),
            size=3) +
  scale_fill_gradient2(low = 'blue',high = 'red',mid = 'white',midpoint = 0) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#'
#' ### Perform regression ###
#'

if(bool_longTermTrend){
  # this is the formula for the univariate regression
  formula_vic_all <- 
    paste0("count ~ ",
           paste0(names_vic_all,collapse = ' + '), 
           " + CNTY_CODE + ob.trend + offset(log(pop_total))")  
}else{
  # this is the formula for the univariate regression
  formula_vic_all <- 
    paste0("count ~ ",
           paste0(names_vic_all,collapse = ' + '), 
           " + CNTY_CODE + offset(log(pop_total))")  
}


# call glm
glm_multiple <- 
  stats::glm(formula = 
               as.formula(formula_vic_all),
             family = quasipoisson(link='log'), 
             data = df_cnt_week_subset)

# compute pvalues and SE with lmtest::coeftest
## Poisson model with SE estimated via robust variance estimator
# z test
glm_coeftest_multiple <- lmtest::coeftest(glm_multiple,vcov=sandwich)

# extract regression coefficient, se and p-value
df_glm_multiple <- 
  data.frame(var_lag = row.names(glm_coeftest_multiple),
             beta = as.numeric(glm_coeftest_multiple[,'Estimate']),
             se = as.numeric(glm_coeftest_multiple[,'Std. Error']),
             p_value = as.numeric(glm_coeftest_multiple[,'Pr(>|z|)']),
             row.names = row.names(glm_coeftest_multiple),
             stringsAsFactors = F)

# select only rows corresponding to vic variables
df_glm_multiple <-
  df_glm_multiple[grep(pattern = 'vic',x = df_glm_multiple$var_lag),] 



# add confidence intervals
# takes about 5 min
test <- proc.time()
conf_int <- confint(glm_multiple, names_vic_all, level = 0.95)
proc.time() - test

# add to df_glm_multiple 
df_glm_multiple[names_vic_all,'beta_low'] <- conf_int[,'2.5 %']
df_glm_multiple[names_vic_all,'beta_high'] <- conf_int[,'97.5 %']

# define name and lag
df_glm_multiple$var <-   
  unlist(lapply(X = strsplit(x = df_glm_multiple$var_lag,split = '_'),
                FUN = function(l) paste0('vic_',l[[2]])))
df_glm_multiple$lag <- 
  as.numeric(unlist(lapply(X = strsplit(x = df_glm_multiple$var_lag,split = '_'),
                           FUN = function(l) l[[4]])))

#'
#' append results to main dataframe 
#'

df_glm_multiple$regression_type <- "multiple"

# remove row names from both dataframes
rownames(df_glm_res) <- NULL
rownames(df_glm_multiple) <- NULL

df_glm_res <- rbind(df_glm_res,
                    df_glm_multiple[,names(df_glm_res)])

rm(df_glm_multiple)

df_glm_res$regression_type <- 
  factor(x = df_glm_res$regression_type,levels = c("simple","multiple"))

#'
#' ## Now plot ##
#'

pd <- position_dodge(width = 0.4)

g_beta <-
  ggplot(data = df_glm_res,
         mapping = aes(x=as.numeric(lag),y=beta,
                       col=regression_type)) +
  geom_point(aes(shape=regression_type),position = pd) +
  geom_errorbar(mapping = aes(ymin=beta_low,ymax=beta_high),
                position = pd) +
  geom_hline(mapping = aes(yintercept=0),linetype="dashed") +
  labs(x='lag (in weeks)',y = expression(beta),
       col="regression type",shape="regression type") +
  facet_wrap( ~ var,ncol=1,scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top")

g_beta

pdf(file = paste0('g_beta_',suffix_pdf,'.pdf'),width = 7,height = 5)
g_beta
dev.off()


#'
#' # Plot rate ratios rather than beta values #
#'

#'
#' Calculate rate ratios between quartiles
#'

# calculate quartiles for each variable

df_quartiles <-
  data.frame(
    var=names_vic,
    var_quart1=unlist(lapply(
      X = df_cnt_week_subset[,paste0(names_vic,'_lag_0_weeks')],
      FUN = function(col) quantile(col,probs=c(0.25),na.rm = T))),
    var_quart3=unlist(lapply(
      X = df_cnt_week_subset[,paste0(names_vic,'_lag_0_weeks')],
      FUN = function(col) quantile(col,probs=c(0.75),na.rm = T))),
    row.names = NULL,
    stringsAsFactors = F)

print(df_quartiles)

# join quartile values to dataframe containing regression results
df_glm_res <-
  dplyr::left_join(x = df_glm_res,
                   y = df_quartiles,
                   by = "var")


# calculate interquartile range
df_glm_res <-
  df_glm_res %>%
  mutate(var_interquart = var_quart3 - var_quart1)

# add mean, lower and upper rate ratios to the dataframe
# rate ratio is exp(Delta beta_j)
df_glm_res <-
  df_glm_res %>%
  mutate(RR = exp(var_interquart * beta),
         RR_low = exp(var_interquart * beta_low),
         RR_high = exp(var_interquart * beta_high))

#'
#' now plot
#'

g_RR <-
  ggplot(data = df_glm_res,
         mapping = aes(x=as.numeric(lag),y=RR,
                       col=regression_type)) +
  geom_point(aes(shape=regression_type),position = pd) +
  geom_errorbar(mapping = aes(ymin=RR_low,ymax=RR_high),
                position = pd) +
  geom_hline(mapping = aes(yintercept=1),linetype="dashed") +
  labs(x='lag (in weeks)',y = "incidence rate ratio (IRR)",
       col="regression type",shape="regression type") +
  facet_wrap( ~ var,ncol=1,scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top")

g_RR

pdf(file = paste0('g_RR_',suffix_pdf,'.pdf'),width = 7,height = 5)
g_RR
dev.off()

g_RR_multipleOnly <-
  ggplot(data = subset(x = df_glm_res,
                       subset = regression_type == "multiple"),
         mapping = aes(x=as.numeric(lag),y=RR)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin=RR_low,ymax=RR_high)) +
  geom_hline(mapping = aes(yintercept=1),linetype="dashed") +
  labs(x='lag (in weeks)',y = "incidence rate ratio (IRR)") +
  facet_wrap( ~ var,ncol=1,scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top")

g_RR_multipleOnly

saveRDS(object = g_RR_multipleOnly,file = paste0('g_RR_multipleOnly_',suffix_pdf,'.rds'))


pdf(file = paste0('g_RR_multipleOnly_',suffix_pdf,'.pdf'),width = 5,height = 4)
g_RR_multipleOnly
dev.off()

#'
#' # print table and values for convenience #
#'


# this is just to make my life easier for copy-pasting

nbDigits <- 2

df_glm_res$text_beta <- 
  paste0(round(df_glm_res$beta,nbDigits),
         ' (',round(df_glm_res$beta_low,nbDigits),
         '-',round(df_glm_res$beta_high,nbDigits),')')

# this is just to make my life easier for copy-pasting
df_glm_res$text_RR <- 
  paste0(round(df_glm_res$RR,nbDigits),
         ' (',round(df_glm_res$RR_low,nbDigits),
         '-',round(df_glm_res$RR_high,nbDigits),')')

df_glm_res$text_p_value <-
  round(df_glm_res$p_value,nbDigits)

rownames(df_glm_res) <- 
  paste0(df_glm_res$var,'_',
         df_glm_res$regression_type,
         '_',df_glm_res$lag)

print(df_glm_res[,c('var','regression_type','lag',
                    "var_quart1","var_quart3","var_interquart", 
                    'text_beta','text_RR','text_p_value','p_value')])

write.table(x = df_glm_res[,c('var','regression_type','lag',
                              "var_quart1","var_quart3","var_interquart", 
                              'text_beta','text_RR','text_p_value','p_value')],
            file = paste0('table_coefficients_',suffix_pdf,'.csv'),sep = ',')


