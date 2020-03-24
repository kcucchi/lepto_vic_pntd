#' ---
#' title: "Mediation Analysis"
#' author: "Karina Cucchi"
#' date: "March 15th, 2018"
#' ---
#' 
#' This script is the implementation 
#' of the mediation analysis 
#' as described in 2018-03-15_mediation_analysis.docx
#' 
#' 

library(dlnm)
library(sandwich)
library(lmtest)

library(dplyr)

library(MASS) # for glm confidence intervals (confint)

library(ggplot2)
library(cowplot)

#' 
#' # Load and format data #
#' 

#'
#' ## Load data ##
#'

# read yearly-county resolution
df_cnt_year <- 
  readRDS(file = '../../../data_lepto/2_formatted_data/df_cnt_year_final.rds')
str(df_cnt_year)

#'
#' ## refine by occupation : limit to farmers ##
#'

df_cnt_year$count <- df_cnt_year$count_farmers
head(df_cnt_year)

#'
#' ## refine in space ##
#'

# df_cnt_year <- 
#   subset(df_cnt_year,cntWithLep_farmers)

# calculate the percentage of zeros
sum(df_cnt_year$count==0,na.rm = T)/nrow(df_cnt_year)


#'
#' ## quick fix in time ##
#'

# # discard year 2004 (because temperature is weird for year 2003)
# # (need to fix temperature for year 2003)
# df_cnt_year <- 
#   subset(x = df_cnt_year,subset = (year!="2003"))

#'
#' ## check statistics of data ##
#' 

hist(df_cnt_year$count+1)
mean(df_cnt_year$count+1,na.rm = T)
var(df_cnt_year$count+1,na.rm = T)
# still overdipersion in data...

# so here I don't understand how I can have stable results actually
# I am trying to predict zeros using a log-model...
# Shouldn't I try to predict counts+1?

hist(log(df_cnt_year$count+1))
mean(log(df_cnt_year$count+1),na.rm = T)
var(log(df_cnt_year$count+1),na.rm = T)

## looks like if I consider log-(counts+1), Poisson is ok?

#'
#' Types of analysis : 
#' simple/multiple regressions, within year/with previous year
#'

# # prepare dataframe that will contain regression results
names_res <- c("var","regression_type","withLag","lag",
               "beta","se","beta_low","beta_high","p_value",
               "withMediating")
df_glm_res <- data.frame(matrix(NA,ncol=length(names_res),nrow=0))
names(df_glm_res) <- names_res;rm(names_res)

#'
#' Define vic variables of interest in analysis
#'

# names_vic contains the names of vic variables to include in the regressions
# suffix_pdf is the character to append at the end of pdf names

# names_vic <-
#   names(df_cnt_year)[grep(pattern = "^vic_",x = names(df_cnt_year))]
# suffix_pdf <- "all"
# names_vic = c('vic_Prcp','vic_roff','vic_sm1');suffix_pdf="hydro"
# names_vic = c('vic_roff','vic_sm1');suffix_pdf="roff,sm1"
# names_vic = c('vic_roff','vic_sm1','vic_Tmax');suffix_pdf="roff,sm1,Tmax"
names_vic = c('vic_Prcp','vic_Tmin');
# names_vic = c("vic_sm1","vic_sm2","vic_sm3","vic_roff",
#               "vic_Prcp","vic_Tmin");suffix_pdf="hydro,Tmin"
# names_vic = c("vic_Prcp","vic_sm1","vic_Tmin");suffix_pdf="Prcp,sm1,Tmin"

mediator_vic="vic_sm1"

suffix_pdf <- paste(c(paste(names_vic,collapse = ','),
                      mediator_vic),
                    collapse = '_')

#'
#' # Subset (if stratify) #
#'

summary(df_cnt_year$vic_Tmin)

quantile(df_cnt_year$vic_Tmin,probs = c(0.33,0.66),na.rm = T)

df_cnt_year <- subset(df_cnt_year,vic_Tmin >= 14.5)

#'
#' ## perform multiple regression without mediator ##
#'

glm_multiv_noMediator <-
  stats::glm(formula = 
               as.formula(paste0("count ~ ",
                                 paste(names_vic,collapse = ' + '), 
                                 " + CNTY_CODE + offset(log(pop_agr))")),
             family = quasipoisson(link='log'), 
             data = df_cnt_year)

# compute pvalues and SE with lmtest::coeftest
## Poisson model with SE estimated via robust variance estimator
# z test
# keep only values for vic parameters
glm_coeftest_multiv_noMediator <- 
  lmtest::coeftest(glm_multiv_noMediator,vcov=sandwich)[names_vic,]

# store as dataframe
df_glm_multiv_noMediator <- 
  as.data.frame(glm_coeftest_multiv_noMediator)
# remove z value
df_glm_multiv_noMediator <- 
  df_glm_multiv_noMediator[,setdiff(names(df_glm_multiv_noMediator),'z value')]
# rename columns
names(df_glm_multiv_noMediator) <- 
  c('beta','se','p_value')
# add value for variable
df_glm_multiv_noMediator$var <- 
  row.names(df_glm_multiv_noMediator)

conf_int <- confint(glm_multiv_noMediator, names_vic, level = 0.95)

df_glm_multiv_noMediator <-
  df_glm_multiv_noMediator %>%
  mutate(beta_low = conf_int[,'2.5 %'],
         beta_high = conf_int[,'97.5 %'])

# calculate confidence intervals
# df_glm_multiv_noMediator <-
#   df_glm_multiv_noMediator %>%
#   mutate(beta_low = beta - 1.96 * se,
#          beta_high = beta + 1.96 * se)

#'
#' append results to main dataframe
#'

df_glm_res <-
  rbind(df_glm_res,
        data.frame(df_glm_multiv_noMediator,
                   regression_type="multiple",
                   withLag=F,
                   lag=NA,
                   withMediating=F)[,names(df_glm_res)])

rm(df_glm_multiv_noMediator,glm_coeftest_multiv_noMediator,glm_multiv_noMediator)


print(df_glm_res)

# set desired dodge width
pd <- position_dodge(width = 0.4)

vic_names2labs <-
  c("vic_sm1"=expression(theta[1]),
    "vic_sm2"=expression(theta[2]),
    "vic_sm3"=expression(theta[3]),
    "vic_Prcp"="P",
    "vic_Tmin"=expression("T"[min]),
    "vic_Tmax"=expression("T"[max]),
    "vic_roff"="Q")

g_beta_noMediator <-
  ggplot(data=df_glm_res,aes(x=var,y=beta,col=regression_type)) +
  geom_point(aes(shape=regression_type),position = pd) +
  geom_errorbar(mapping = aes(ymin=beta_low,ymax=beta_high),position = pd) +
  geom_hline(mapping = aes(yintercept=0),linetype="dashed") +
  labs(x="risk factor",y="with mediator",
       col="regression\ntype",shape="regression\ntype") +
  scale_x_discrete(labels = vic_names2labs) +
  theme_bw()

g_beta_noMediator




#'
#' # Now include mediator #
#'

# see code in
# https://rpubs.com/kaz_yos/poisson 
# also see reference Cameron and Trivedi (2009) for robust estimators in
# https://stats.idre.ucla.edu/r/dae/poisson-regression/


#'
#' ## perform multiple regression ##
#'

glm_multiv_withMediator <-
  stats::glm(formula =
               as.formula(paste0("count ~ ",
                                 paste(c(names_vic,mediator_vic),collapse = ' + '),
                                 " + CNTY_CODE + offset(log(pop_agr))")),
             family = quasipoisson(link='log'),
             data = df_cnt_year)

# compute pvalues and SE with lmtest::coeftest
## Poisson model with SE estimated via robust variance estimator
# z test
# keep only values for vic parameters
glm_coeftest_multiv_withMediator <-
  lmtest::coeftest(glm_multiv_withMediator,vcov=sandwich)[c(names_vic,mediator_vic),]

# store as dataframe
df_glm_multiv_withMediator <-
  as.data.frame(glm_coeftest_multiv_withMediator)
# remove z value
df_glm_multiv_withMediator <-
  df_glm_multiv_withMediator[,setdiff(names(df_glm_multiv_withMediator),'z value')]
# rename columns
names(df_glm_multiv_withMediator) <-
  c('beta','se','p_value')
df_glm_multiv_withMediator$var <- 
  row.names(df_glm_multiv_withMediator)


conf_int <- confint(glm_multiv_withMediator, 
                    paste(c(names_vic,mediator_vic)), level = 0.95)

df_glm_multiv_withMediator <-
  df_glm_multiv_withMediator %>%
  mutate(beta_low = conf_int[,'2.5 %'],
         beta_high = conf_int[,'97.5 %'])



#' 
#' add results to same dataframe
#'

df_glm_res <-
  rbind(df_glm_res,
        data.frame(df_glm_multiv_withMediator,
                   regression_type="multiple",
                   withLag=F,
                   lag=NA,
                   withMediating=T)[,names(df_glm_res)])

rm(df_glm_multiv_withMediator)

#'
#' now plot all results for analysis with lags
#'

# df_glm_res <-
#   df_glm_res %>%
#   mutate(lag_type=paste0(lag,'_',regression_type))

g_regcoefs <-
  ggplot(data=df_glm_res,
         mapping = aes(x=var,y=beta,
                       col=withMediating,group=withMediating)) +
  geom_point(aes(shape=withMediating),position = pd) +
  geom_errorbar(mapping = aes(ymin=beta_low,ymax=beta_high),
                position = pd) +
  geom_hline(mapping = aes(yintercept=0),linetype="dashed") +
  labs(x="risk factor",y='regression coefficient') +
  scale_x_discrete(labels = vic_names2labs) +
  scale_color_manual(name="equation",
                     values=c("#999999", "#E69F00"),
                     labels=c("without mediator","with mediator")) +
  scale_shape_manual(name="equation",
                    values=c(16,17),
                    labels=c("without mediator","with mediator")) +
  theme_bw() 

g_regcoefs


pdf(file = paste0('g_regcoefs_',suffix_pdf,'.pdf'),width=4,height=2.5)
print(g_regcoefs)
dev.off()

# actually plot rate ratios
# dev.off()
# pdf(paste0('g_beta_all_lag',suffix_pdf,'.pdf'),width=6,height=3)
# g_beta_all
# dev.off()

#'
#' # Post-processing #
#'


#'
#' ## Plot rate ratios rather than beta values ##
#'

#'
#' ### Calculate rate ratios ###
#'

#'
#' First calculate interquartiles for vic variables
#'

# calculate quartiles for each variable
df_quartiles <-
  data.frame(
    var=c(names_vic,mediator_vic),
    var_quart1=unlist(lapply(
      X = df_cnt_year[,c(names_vic,mediator_vic)],
      FUN = function(col) quantile(col,probs=c(0.25),na.rm = T))),
    var_quart3=unlist(lapply(
      X = df_cnt_year[,c(names_vic,mediator_vic)],
      FUN = function(col) quantile(col,probs=c(0.75),na.rm = T))),
    stringsAsFactors = F)

# join quartile values to dataframe containing regression results
df_glm_res <-
  dplyr::left_join(x = df_glm_res,
                   y = df_quartiles,
                   by = "var")

# calculate interquartile range
df_glm_res <-
  df_glm_res %>%
  mutate(var_interquart = var_quart3 - var_quart1)

#'
#' Now calculate rate ratios from interquartile ranges
#'

df_glm_res <-
  df_glm_res %>%
  mutate(RR = exp(var_interquart * beta),
         RR_low = exp(var_interquart * beta_low),
         RR_high = exp(var_interquart * beta_high))


#'
#' ### now plot ###
#'

g_RR <-
  ggplot(data=df_glm_res,
         mapping = aes(x=var,y=RR,
                       col=withMediating,group=var)) +
  geom_point(aes(shape=withMediating),position = pd) +
  geom_errorbar(mapping = aes(ymin=RR_low,ymax=RR_high),
                position = pd) +
  geom_hline(mapping = aes(yintercept=1),linetype="dashed") +
  labs(x="risk factor",y="with mediator") +
  scale_x_discrete(labels = vic_names2labs) +
  scale_y_continuous(trans = "log10",
                     breaks = c(0.1*(1:9),1:10,10*(2:10),100*(2:10)),
                     labels = c(0.1,rep('',3),
                                0.5,rep('',4),
                                1,rep('',3),
                                5,rep('',4),
                                10,rep('',3),
                                50,rep('',4),
                                100,rep('',3),
                                500,rep('',4),1000)) +
  scale_color_manual(name="equation",
                     values=c("#999999", "#E69F00"),
                     labels=c("without mediator","with mediator")) +
  scale_shape_manual(name="equation",
                     values=c(16,17),
                     labels=c("without mediator","with mediator")) +
  labs(x="risk factor",
       y="incidence rate ratio (IRR)") +
  theme_bw() 

g_RR

pdf(file = paste0('g_RR_',suffix_pdf,'.pdf'),width=5,height=3)
print(g_RR)
dev.off()



#'
#' ## print table and values for convenience ##
#'

# this is just to make my life easier for copy-pasting
df_glm_res$text_beta <- 
  paste0(round(df_glm_res$beta,3),
         ' (',round(df_glm_res$beta_low,3),
         ';',round(df_glm_res$beta_high,3),')')

# this is just to make my life easier for copy-pasting
df_glm_res$text_RR <- 
  paste0(round(df_glm_res$RR,3),
         ' (',round(df_glm_res$RR_low,3),
         ';',round(df_glm_res$RR_high,3),')')

df_glm_res$text_p_value <-
  round(df_glm_res$p_value,3)

rownames(df_glm_res) <- 
  paste0(df_glm_res$var,'_',
         df_glm_res$regression_type,
         '_',df_glm_res$withMediating)

print(df_glm_res[,c('var','regression_type','withLag','lag',
                    'text_beta','text_RR','text_p_value','p_value')])





