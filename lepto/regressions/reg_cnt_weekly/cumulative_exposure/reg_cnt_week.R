
#' ---
#' title: "Regression of lepto counts wrt risk factors at the weekly-county level"
#' author: "Karina Cucchi"
#' date: "Septamber 28th, 2018"
#' ---

library(splines)
library(pbs)
library(dlnm)
library(ggplot2)
library(MASS)
library(dplyr)

library(sandwich)
library(lmtest)

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
#' ## cross basis precipitation ##
#'

lag_max <- 15 # in weeks

cb_Prcp <-
  dlnm::crossbasis(x = df_cnt_week[,'vic_Prcp'],
                   lag = lag_max,
                   argvar = list(fun="poly",deg=1),  
                   arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                   group = df_cnt_week$CNTY_CODE)
# keep only weeks during high lepto season
cb_Prcp_subset <- cb_Prcp[row_keep,]
cb_attr <- attributes(cb_Prcp)
cb_attr$dim[1] <- nrow(cb_Prcp_subset)
attributes(cb_Prcp_subset) <- cb_attr

#'
#' ## cross basis temperature ##
#'

cb_Tmin <-
  dlnm::crossbasis(x = df_cnt_week[,'vic_Tmin'],
                   lag = lag_max,
                   argvar = list(fun="poly",deg=1),  
                   # arglag = list(fun="ns",knots=c(5,10),intercept=FALSE),
                   arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                   group = df_cnt_week$CNTY_CODE)
# keep only weeks during high lepto season
cb_Tmin_subset <- cb_Tmin[row_keep,]
cb_attr <- attributes(cb_Tmin)
cb_attr$dim[1] <- nrow(cb_Tmin_subset)
attributes(cb_Tmin_subset) <- cb_attr

#'
#' # No soil moisture #
#'

# call glm
# no temperature
glm1 <- 
  stats::glm(formula = 
               # count ~ cb_Prcp_subset + cb_Tmin_subset + 
               count ~ cb_Prcp_subset +
               CNTY_CODE + ob.trend + offset(log(pop_total)),
             family = quasipoisson(link='log'), 
             data = df_cnt_week_subset)

#'
#' ## cumulative precipitation ##
#'

quant_25 <- 
  quantile(x = df_cnt_week_subset[,'vic_Prcp'],na.rm = T)[["25%"]]
quant_75 <- 
  quantile(x = df_cnt_week_subset[,'vic_Prcp'],na.rm = T)[["75%"]]

pred1.Prcp <- 
  crosspred(basis = cb_Prcp_subset, 
            model = glm1, 
            at = quant_75, # constant exposure throughout the lag period
            cen = quant_25, # used as a reference for predictions
            cumul=TRUE)

plot(pred1.Prcp, "slices", var=quant_75, 
     col=3, cumul=FALSE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Lag-response curve")

plot(pred1.Prcp, "slices", var=quant_75, 
     col=2, cumul=TRUE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Cumulative association")

# print cumulative IRR
paste0(round(as.numeric(pred1.Prcp$allRRfit),2),
       ' (',
       round(as.numeric(pred1.Prcp$allRRlow),2),
       '-',
       round(as.numeric(pred1.Prcp$allRRhigh),2),
       ')')

# lagged IRRs
print(round(cbind(unname(pred1.Prcp$matRRfit[1,]),
                  unname(pred1.Prcp$matRRlow[1,]),
                  unname(pred1.Prcp$matRRhigh[1,])),2))

#'
#' ## cumulative temperature ##
#'

# quant_Tmin_25 <- 
#   quantile(x = df_cnt_week_subset[,'vic_Tmin'],na.rm = T)[["25%"]]
# quant_Tmin_75 <- 
#   quantile(x = df_cnt_week_subset[,'vic_Tmin'],na.rm = T)[["75%"]]
# 
# pred1.Tmin <- 
#   crosspred(basis = cb_Tmin_subset, 
#             model = glm1, 
#             at = quant_Tmin_75, # constant exposure throughout the lag period
#             cen = quant_Tmin_25, # used as a reference for predictions
#             cumul=TRUE)
# 
# plot(pred1.Tmin, "slices", var=quant_Tmin_75, 
#      col=3, cumul=FALSE, ylab="Cumulative RR",
#      ci.arg=list(density=15,lwd=2),
#      main="Lag-response curve")
# 
# plot(pred1.Tmin, "slices", var=quant_Tmin_75, 
#      col=2, cumul=TRUE, ylab="Cumulative RR",
#      ci.arg=list(density=15,lwd=2),
#      main="Cumulative association")
# 
# # print cumulative IRR
# paste0(round(as.numeric(pred1.Tmin$allRRfit),2),
#        ' (',
#        round(as.numeric(pred1.Tmin$allRRlow),2),
#        '-',
#        round(as.numeric(pred1.Tmin$allRRhigh),2),
#        ')')
# 
# # print lagged IRRs
# print(round(cbind(unname(pred1.Tmin$matRRfit[1,]),
#                   unname(pred1.Tmin$matRRlow[1,]),
#                   unname(pred1.Tmin$matRRhigh[1,])),2))

#'
#' # With soil moisture #
#'

cb_sm1 <-
  dlnm::crossbasis(x = df_cnt_week[,'vic_sm1'],
                   lag = lag_max,
                   argvar = list(fun="poly",deg=1),  
                   arglag = list(fun="integer"), #unconstrained (no smoothing with time)
                   group = df_cnt_week$CNTY_CODE)
# keep only weeks during high lepto season
cb_sm1_subset <- cb_sm1[row_keep,]
cb_attr <- attributes(cb_sm1)
cb_attr$dim[1] <- nrow(cb_sm1_subset)
attributes(cb_sm1_subset) <- cb_attr

# call glm
glm2 <- 
  stats::glm(formula = 
               # count ~ cb_Prcp_subset + cb_sm1_subset + cb_Tmin_subset + 
               count ~ cb_Prcp_subset + cb_sm1_subset + 
               CNTY_CODE + ob.trend + offset(log(pop_total)),
             family = quasipoisson(link='log'), 
             data = df_cnt_week_subset)

#'
#' ## precipitation ##
#'

pred2.Prcp <- 
  crosspred(basis = cb_Prcp_subset, 
            model = glm2, 
            at = quant_75, # constant exposure throughout the lag period
            cen = quant_25, # used as a reference for predictions
            cumul=TRUE)

plot(pred2.Prcp, "slices", var=quant_75, 
     col=3, cumul=FALSE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Lag-response curve")

plot(pred2.Prcp, "slices", var=quant_75, 
     col=2, cumul=TRUE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Cumulative association")


paste0(round(as.numeric(pred2.Prcp$allRRfit),2),
       ' (',
       round(as.numeric(pred2.Prcp$allRRlow),2),
       '-',
       round(as.numeric(pred2.Prcp$allRRhigh),2),
       ')')

# print lagged IRRs
print(round(cbind(unname(pred2.Prcp$matRRfit[1,]),
                  unname(pred2.Prcp$matRRlow[1,]),
                  unname(pred2.Prcp$matRRhigh[1,])),2))


#'
#' ## soil moisture ##
#'

quant_sm1_25 <- 
  quantile(x = df_cnt_week_subset[,'vic_sm1'],na.rm = T)[["25%"]]
quant_sm1_75 <- 
  quantile(x = df_cnt_week_subset[,'vic_sm1'],na.rm = T)[["75%"]]

pred2.sm <- 
  crosspred(basis = cb_sm1_subset, 
            model = glm2, 
            at = quant_sm1_75, # constant exposure throughout the lag period
            cen = quant_sm1_25, # used as a reference for predictions
            cumul=TRUE)

plot(pred2.sm, "slices", var=quant_sm1_75, 
     col=3, cumul=FALSE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Lag-response curve")

plot(pred2.sm, "slices", var=quant_sm1_75, 
     col=2, cumul=TRUE, ylab="Cumulative RR",
     ci.arg=list(density=15,lwd=2),
     main="Cumulative association")


paste0(round(as.numeric(pred2.sm$allRRfit),2),
       ' (',
       round(as.numeric(pred2.sm$allRRlow),2),
       '-',
       round(as.numeric(pred2.sm$allRRhigh),2),
       ')')

# print lagged IRRs
print(round(cbind(unname(pred2.sm$matRRfit[1,]),
                  unname(pred2.sm$matRRlow[1,]),
                  unname(pred2.sm$matRRhigh[1,])),2))

#'
#' ## temperature ##
#'

# pred2.Tmin <- 
#   crosspred(basis = cb_Tmin_subset, 
#             model = glm2, 
#             at = quant_Tmin_75, # constant exposure throughout the lag period
#             cen = quant_Tmin_25, # used as a reference for predictions
#             cumul=TRUE)
# 
# plot(pred2.Tmin, "slices", var=quant_Tmin_75, 
#      col=3, cumul=FALSE, ylab="Cumulative RR",
#      ci.arg=list(density=15,lwd=2),
#      main="Lag-response curve")
# 
# plot(pred2.Tmin, "slices", var=quant_Tmin_75, 
#      col=2, cumul=TRUE, ylab="Cumulative RR",
#      ci.arg=list(density=15,lwd=2),
#      main="Cumulative association")
# 
# 
# paste0(round(as.numeric(pred2.Tmin$allRRfit),2),
#        ' (',
#        round(as.numeric(pred2.Tmin$allRRlow),2),
#        '-',
#        round(as.numeric(pred2.Tmin$allRRhigh),2),
#        ')')
# 
# # print lagged IRRs
# print(round(cbind(unname(pred2.Tmin$matRRfit[1,]),
#                   unname(pred2.Tmin$matRRlow[1,]),
#                   unname(pred2.Tmin$matRRhigh[1,])),2))
