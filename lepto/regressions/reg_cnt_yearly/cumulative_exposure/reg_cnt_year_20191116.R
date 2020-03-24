#' ---
#' title: "Regression of lepto counts wrt risk factors at the yearly-county level"
#' author: "Karina Cucchi"
#' date: "September 27th, 2018"
#' ---
#' 
#' Calculate cumulative exposure for paper (suggested by Phil)
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

# df_cnt_year$count <- df_cnt_year$count_farmers
# head(df_cnt_year)

#'
#' ## refine in space ##
#'

df_cnt_year <-
  subset(df_cnt_year,cntWithLep)

# calculate the percentage of zeros
sum(df_cnt_year$count==0,na.rm = T)/nrow(df_cnt_year)


#'
#' ## quick fix in time ##
#'

#'
#' ## change CNTY_CODE to factors ##
#'

# do it after removing certain counties to avoid having factors with zero counts
# (this messes up the cross-basis function)

df_cnt_year$CNTY_CODE <- 
  as.factor(df_cnt_year$CNTY_CODE)

# check how many counties
length(levels(df_cnt_year$CNTY_CODE))


#'
#' ## Calculate lagged values ##
#'

# calculate lags values of predictors
# using dlnm package to take advantage of grouping by county

# names_vic = c("vic_sm1","vic_roff",
#               "vic_Prcp","vic_Tmin")
# 
# for(i_vic in 1:length(names_vic)){
#   
#   # does the lag by county
#   cb_iVic <-
#     dlnm::crossbasis(x = df_cnt_year[,names_vic[i_vic]],
#                      lag = 1,
#                      argvar=list(fun="lin"),     # no transform in predictor space
#                      arglag=list(fun="integer"), # unconstrained in lag space
#                      group = df_cnt_year$CNTY_CODE)
#   
#   #v1.l1 is lag 0 year and v1.l2 is lag 2 years -> we want v1.l2
#   df_cnt_year[,paste0(names_vic[i_vic],'_lag')] <- cb_iVic[,'v1.l2']
#   
#   rm(cb_iVic)
#   
# }



#' 
#' # No soil moisture #
#'

# i_vic=3

cb_Prcp <-
  dlnm::crossbasis(x = df_cnt_year[,'vic_Prcp'],
                   lag = 1,
                   argvar=list(fun="lin"),     # no transform in predictor space
                   arglag=list(fun="integer"), # unconstrained in lag space
                   group = df_cnt_year$CNTY_CODE)

glm_i_config <- 
  stats::glm(formula = 
               # as.formula("count ~ cb_Prcp + vic_Tmin + 
               as.formula("count ~ cb_Prcp +
                          CNTY_CODE + offset(log(pop_total))"),
             family = quasipoisson(link='log'), 
             data = df_cnt_year)

quant_25 <- quantile(x = df_cnt_year[,'vic_Prcp'],na.rm = T)[["25%"]]
quant_75 <- quantile(x = df_cnt_year[,'vic_Prcp'],na.rm = T)[["75%"]]

pred1.pm <- 
  crosspred(basis = cb_Prcp, 
            model = glm_i_config, 
            at = quant_75, # constant exposure throughout the lag period
            cen = quant_25, # used as a reference for predictions
            cumul=TRUE)

plot(pred1.pm, "slices", var=quant_75, 
     col=2, cumul=TRUE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Cumulative association")

# 1 is the maximum lag here, 
# so the following represents the overall cumulative effect
paste0(round(as.numeric(pred1.pm$allRRfit),2),
       ' (',
       round(as.numeric(pred1.pm$allRRlow),2),
       '-',
       round(as.numeric(pred1.pm$allRRhigh),2),
       ')')



#' 
#' # With soil moisture #
#'

# # i_vic=3
# # 
# # cb_Prcp <-
# #   dlnm::crossbasis(x = df_cnt_year[,'vic_Prcp'],
# #                    lag = 1,
# #                    argvar=list(fun="lin"),     # no transform in predictor space
# #                    arglag=list(fun="integer"), # unconstrained in lag space
# #                    group = df_cnt_year$CNTY_CODE)
# 
# glm_i_config_sm <- 
#   stats::glm(formula = 
#                as.formula("count ~ cb_Prcp + vic_Tmin + vic_sm1 +
#                           CNTY_CODE + offset(log(pop_total))"),
#              family = quasipoisson(link='log'), 
#              data = df_cnt_year)
# 
# # quant_25 <- quantile(x = df_cnt_year[,names_vic[i_vic]],na.rm = T)[["25%"]]
# # quant_75 <- quantile(x = df_cnt_year[,names_vic[i_vic]],na.rm = T)[["75%"]]
# 
# pred1.pm_sm <- 
#   crosspred(basis = cb_Prcp, 
#             model = glm_i_config_sm, 
#             at = quant_75, # constant exposure throughout the lag period
#             cen = quant_25, # used as a reference for predictions
#             cumul=TRUE)
# 
# plot(pred1.pm_sm, "slices", var=quant_75, 
#      col=2, cumul=TRUE, ylab="Cumulative RR",
#      ci.arg=list(density=15,lwd=2),
#      main="Cumulative association")
# 
# 
# paste0(round(as.numeric(pred1.pm_sm$allRRfit),2),
#        ' (',
#        round(as.numeric(pred1.pm_sm$allRRlow),2),
#        '-',
#        round(as.numeric(pred1.pm_sm$allRRhigh),2),
#        ')')


#' 
#' # Full model (with lagged soil moisture) #
#'


cb_sm <-
  dlnm::crossbasis(x = df_cnt_year[,'vic_sm1'],
                   lag = 1,
                   argvar=list(fun="lin"),     # no transform in predictor space
                   arglag=list(fun="integer"), # unconstrained in lag space
                   group = df_cnt_year$CNTY_CODE)

glm_i_config_sm <- 
  stats::glm(formula = 
               # as.formula("count ~ cb_Prcp + cb_sm + vic_Tmin + 
               as.formula("count ~ cb_Prcp + cb_sm +
                          CNTY_CODE + offset(log(pop_total))"),
             family = quasipoisson(link='log'), 
             data = df_cnt_year)

#'
#' Cumulative precipitation
#'

pred1.Prcp <- 
  crosspred(basis = cb_Prcp, 
            model = glm_i_config_sm, 
            at = quant_75, # constant exposure throughout the lag period
            cen = quant_25, # used as a reference for predictions
            cumul=TRUE)

plot(pred1.Prcp, "slices", var=quant_75, 
     col=2, cumul=TRUE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Cumulative association")


paste0(round(as.numeric(pred1.Prcp$allRRfit),2),
       ' (',
       round(as.numeric(pred1.Prcp$allRRlow),2),
       '-',
       round(as.numeric(pred1.Prcp$allRRhigh),2),
       ')')

#'
#' Cumulative soil moisture
#'

quant_sm_25 <- 
  quantile(x = df_cnt_year[,'vic_sm1'],na.rm = T)[["25%"]]
quant_sm_75 <- 
  quantile(x = df_cnt_year[,'vic_sm1'],na.rm = T)[["75%"]]


pred1.sm <- 
  crosspred(basis = cb_sm, 
            model = glm_i_config_sm, 
            at = quant_sm_75, # constant exposure throughout the lag period
            cen = quant_sm_25, # used as a reference for predictions
            cumul=TRUE)

plot(pred1.sm, "slices", var=quant_sm_75, 
     col=2, cumul=TRUE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Cumulative association")


paste0(round(as.numeric(pred1.sm$allRRfit),2),
       ' (',
       round(as.numeric(pred1.sm$allRRlow),2),
       '-',
       round(as.numeric(pred1.sm$allRRhigh),2),
       ')')
