
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
library(stringr)

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

# remove years in 2013 and 2014 (no vic data)
df_cnt_week <- 
  subset(x = df_cnt_week,y < 2013)


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
# ### THIS ONE IS IN THE PAPER ###
# names_vic = c('vic_Prcp','vic_sm1','vic_Tmin');suffix_pdf="Prcp,sm1,Tmin"
# -- same but without temperature as a control
# names_vic = c('vic_Prcp','vic_sm1');suffix_pdf="Prcp,sm1"
# names_vic = c('vic_Prcp','vic_roff','vic_Tmin');suffix_pdf="Prcp,roff,Tmin"
# ### THIS ONE IS IN THE PAPER ###
names_vic = c('vic_Prcp','vic_Tmin');suffix_pdf="Prcp,Tmin"
# -- same but without temperature as a control
# names_vic = c('vic_Prcp');suffix_pdf="Prcp"

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

#'
#' ## Environmental covariates ##
#'

# get rid of other vic variables in df_cnt_week
# df_cnt_week <- 
#   df_cnt_week[,setdiff(names(df_cnt_week),
#                        setdiff(names(df_cnt_week)[grep(pattern = "^vic_",
#                                                        x = names(df_cnt_week))],
#                                names_vic))]


lag_max <- 15 # in weeks

# create lagged variables


# for precipitation
cb_vic_Prcp <-  dlnm::crossbasis(x = df_cnt_week$vic_Prcp,
                                 lag = lag_max,
                                 argvar = list(fun="lin"),  
                                 arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                                 group = df_cnt_week$CNTY_CODE)
# now some rows we don't need, the ones corresponding to weeks
# outside of the lepto season. find out what these are based on
# values in df_cnt_week
# row_keep has been defined earlier but I redefine it here in case
row_keep <- df_cnt_week$m %in% 8:10
cb_vic_Prcp_subset <- subset(cb_vic_Prcp, row_keep)


# this transformed cb in an array...........
# now we need to redefine this as a crossbasis object...
# get names attributes that the subset is missing
missing_attribute_names <-
  setdiff(names(attributes(cb_vic_Prcp)), names(attributes(cb_vic_Prcp_subset)))
# loop over attributes of original crossbasis and paste
for(attribute_name in missing_attribute_names){
  attr(cb_vic_Prcp_subset, attribute_name) <- attr(cb_vic_Prcp, attribute_name)
}

# ok...
# now same for runoff, soil moisture and temperature

# runoff
cb_vic_roff <-  dlnm::crossbasis(x = df_cnt_week$vic_roff,
                                 lag = lag_max,
                                 argvar = list(fun="lin"),  
                                 arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                                 group = df_cnt_week$CNTY_CODE)
row_keep <- df_cnt_week$m %in% 8:10
cb_vic_roff_subset <- subset(cb_vic_roff, row_keep)
missing_attribute_names <-
  setdiff(names(attributes(cb_vic_roff)), names(attributes(cb_vic_roff_subset)))
for(attribute_name in missing_attribute_names){
  attr(cb_vic_roff_subset, attribute_name) <- attr(cb_vic_roff, attribute_name)
}

# soil moisture
cb_vic_sm1 <-  dlnm::crossbasis(x = df_cnt_week$vic_sm1,
                                lag = lag_max,
                                argvar = list(fun="lin"),  
                                arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                                group = df_cnt_week$CNTY_CODE)
row_keep <- df_cnt_week$m %in% 8:10
cb_vic_sm1_subset <- subset(cb_vic_sm1, row_keep)
missing_attribute_names <-
  setdiff(names(attributes(cb_vic_sm1)), names(attributes(cb_vic_sm1_subset)))
for(attribute_name in missing_attribute_names){
  attr(cb_vic_sm1_subset, attribute_name) <- attr(cb_vic_sm1, attribute_name)
}

# temperature
cb_vic_Tmin <-  dlnm::crossbasis(x = df_cnt_week$vic_Tmin,
                                 lag = lag_max,
                                 argvar = list(fun="lin"),  
                                 arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                                 group = df_cnt_week$CNTY_CODE)
row_keep <- df_cnt_week$m %in% 8:10
cb_vic_Tmin_subset <- subset(cb_vic_Tmin, row_keep)
missing_attribute_names <-
  setdiff(names(attributes(cb_vic_Tmin)), names(attributes(cb_vic_Tmin_subset)))
for(attribute_name in missing_attribute_names){
  attr(cb_vic_Tmin_subset, attribute_name) <- attr(cb_vic_Tmin, attribute_name)
}


#'
#' # Regression analysis #
#'


# get names of columns containing lagged variables of names_vic
names_vic_all <-
  names(df_cnt_week_subset)[grep(pattern='vic_',x=names(df_cnt_week_subset))]

#'
#' ### check correlation between variables ###
#'

name_var_cor <- "roff"
cb_name <- paste0('cb_vic_', name_var_cor, '_subset')

# compute correlation matrix
df_cnt_week_forCor <-
  as.data.frame(get(cb_name)) 
mat_cor <- cor(df_cnt_week_forCor)

df_cor <- as.data.frame(mat_cor)
df_cor$Var1 <- row.names(df_cor)

df_cor_long <- tidyr::gather(data=df_cor,key='Var2',value='cor',-Var1)

str(df_cor_long)

g_cor_lagged <- 
  ggplot() +
  geom_tile(data = df_cor_long,
            mapping = aes(x = factor(x = Var1, levels = colnames(get(cb_name))),
                          y = factor(x = Var2, levels = colnames(get(cb_name))),
                          fill=cor)) +
  # geom_point(data = subset(df_cor_long,subset = abs(cor) >= 0.90),
  #            mapping = aes(x = Var1,y = Var2), col='grey') +
  geom_text(data = df_cor_long,
            mapping = aes(x = factor(x = Var1, levels = colnames(get(cb_name))),
                          y = factor(x = Var2, levels = colnames(get(cb_name))),
                          label=round(cor,1)),
            size=3) +
  scale_fill_gradient2(low = 'blue',high = 'red',mid = 'white',midpoint = 0) +
  xlab(paste(name_var_cor, 'lagged')) +
  ylab(paste(name_var_cor, 'lagged')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g_cor_lagged

pdf(paste0('g_cor_lagged_', name_var_cor, '.pdf'), height=5, width=6)
print(g_cor_lagged)
dev.off()

df_cor$lag <- as.numeric(
  unlist(lapply(X = strsplit(x = df_cor$Var1, split = '[.]'),
                FUN = function(l) as.numeric(substring(l[[2]], 2, 3)) - 1)))

str(df_glm_multiple)

# https://stackoverflow.com/questions/14266333/extract-confidence-interval-values-from-acf-correlogram
ci <- qnorm((1 + 0.95)/2)/sqrt(16)

g_acf <-
  ggplot(data = df_cor, mapping = aes(x=lag, y=v1.l1)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = ci), linetype = 2, color = 'darkblue') +
  xlab(label = 'lag (in weeks)') +
  ylab(paste0('acf ', name_var_cor)) +
  theme_bw()


pdf(paste0('g_acf_', name_var_cor, '.pdf'), height=3, width=6)
print(g_acf)
dev.off()


#'
#' ### Perform regression ###
#'

formula_vic_all <- 
  paste0("count ~ ",
         paste(paste0('cb_', names_vic, '_subset'), collapse = ' + '), 
         " + CNTY_CODE + ob.trend + offset(log(pop_total))")  


# call glm
glm_multiple <- 
  stats::glm(formula = as.formula(formula_vic_all),
             family = quasipoisson(link='log'), 
             data = df_cnt_week_subset)

# test for collinearity using alias
glm_alias <- alias(glm_multiple)

# compute pvalues and SE with lmtest::coeftest
## Poisson model with SE estimated via robust variance estimator
# z test
glm_coeftest_multiple <- lmtest::coeftest(glm_multiple, vcov=sandwich)

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
conf_int <- confint(
  object = glm_multiple,
  parm = df_glm_multiple$var_lag[grep(pattern = 'vic',
                                      x = df_glm_multiple$var_lag)],
  level = 0.95)
proc.time() - test

# add to df_glm_multiple
df_glm_multiple[,'beta_low'] <- conf_int[, '2.5 %']
df_glm_multiple[,'beta_high'] <- conf_int[, '97.5 %']

#################
# calculate VIF
#################

# calculate VIF values
vif_df <- data.frame(car::vif(glm_multiple))
names(vif_df) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
# rename variables in line with original nomenclature
rownames(vif_df) <- str_remove(string = rownames(vif_df), pattern = 'cb_')

# define name and lag
df_glm_multiple$var <-   
  unlist(lapply(X = strsplit(x = df_glm_multiple$var_lag,split = '_'),
                FUN = function(l) paste0('vic_',l[[3]])))
df_glm_multiple$lag <- 
  as.numeric(unlist(lapply(X = strsplit(x = df_glm_multiple$var_lag,split = '[.]'),
                           FUN = function(l) as.numeric(substring(l[[2]], 2, 3)) - 1)))
str(df_glm_multiple)


#'
#' ## Now plot ##
#'

g_beta <-
  ggplot(data = df_glm_multiple,
         mapping = aes(x=as.numeric(lag),y=beta)) +
  geom_errorbar(mapping = aes(ymin=beta_low,ymax=beta_high)) +
  geom_hline(mapping = aes(yintercept=0),linetype="dashed") +
  labs(x='lag (in weeks)',y = expression(beta)) +
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
      X = df_cnt_week_subset[, names_vic],
      FUN = function(col) quantile(col,probs=c(0.25),na.rm = T))),
    var_quart3=unlist(lapply(
      X = df_cnt_week_subset[, names_vic],
      FUN = function(col) quantile(col,probs=c(0.75),na.rm = T))),
    row.names = NULL,
    stringsAsFactors = F)

print(head(df_quartiles))

# join quartile values to dataframe containing regression results
df_glm_multiple <-
  dplyr::left_join(x = df_glm_multiple,
                   y = df_quartiles,
                   by = "var")


# calculate interquartile range
df_glm_multiple <-
  df_glm_multiple %>%
  mutate(var_interquart = var_quart3 - var_quart1) %>%
  # add mean, lower and upper rate ratios to the dataframe
  # rate ratio is exp(Delta beta_j)
  mutate(RR = exp(var_interquart * beta),
         RR_low = exp(var_interquart * beta_low),
         RR_high = exp(var_interquart * beta_high))

#'
#' now plot
#'

g_RR <-
  ggplot(data = df_glm_multiple,
         mapping = aes(x=as.numeric(lag), y=RR)) +
  geom_errorbar(mapping = aes(ymin=RR_low,ymax=RR_high)) +
  geom_hline(mapping = aes(yintercept=1),linetype="dashed") +
  labs(x='lag (in weeks)',y = "incidence rate ratio (IRR)") +
  facet_wrap( ~ var,ncol=1,scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top")

g_RR

pdf(file = paste0('g_RR_',suffix_pdf,'.pdf'), width = 7, height = 5)
g_RR
dev.off()


#'
#' # print table and values for convenience #
#'


# this is just to make my life easier for copy-pasting

nbDigits <- 2

df_glm_multiple$text_beta <- 
  paste0(round(df_glm_multiple$beta,nbDigits),
         ' (',round(df_glm_multiple$beta_low,nbDigits),
         '-',round(df_glm_multiple$beta_high,nbDigits),')')

# this is just to make my life easier for copy-pasting
df_glm_multiple$text_RR <- 
  paste0(round(df_glm_multiple$RR,nbDigits),
         ' (',round(df_glm_multiple$RR_low,nbDigits),
         '-',round(df_glm_multiple$RR_high,nbDigits),')')

df_glm_multiple$text_p_value <-
  round(df_glm_multiple$p_value,nbDigits)

rownames(df_glm_multiple) <- 
  paste0(df_glm_multiple$var,'_',
         df_glm_multiple$regression_type,
         '_',df_glm_multiple$lag)

print(df_glm_multiple[,c('var','lag',
                         "var_quart1","var_quart3","var_interquart", 
                         'text_beta','text_RR','text_p_value','p_value')])

write.table(x = df_glm_multiple[,c('var','lag',
                                   "var_quart1","var_quart3","var_interquart", 
                                   'text_beta','text_RR','text_p_value','p_value')],
            file = paste0('table_coefficients_',suffix_pdf,'_vif.csv'), sep = ',')


