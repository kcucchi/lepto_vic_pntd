
library(splines)
library(pbs)
library(dlnm)

#'
#' # Get and format data #
#'

#'
#' ## get data ##
#'

# get data at weekly and township resolution
df_cnt_weekly <- readRDS('../df_cnt_weekly_gdb.rds')

head(df_cnt_weekly)

#'
#' ## keep only counties with no missing population
#'

df_cnt_weekly <- subset(df_cnt_weekly, missing_twn_pop == F)

# (reduces the number of counties with at least one case 
# from 99 to 70)

#'
#' ## refine by occupation : limit to farmers ##
#'

df_cnt_weekly$count <- df_cnt_weekly$count_Farmer

#'
#' ## refine in space ##
#'

# check only counties with at least one lep case
df_cnt_weekly_subset <- 
  subset(df_cnt_weekly,countyWithLep)

# calculate the percentage of zeros
sum(df_cnt_weekly_subset$count==0)/nrow(df_cnt_weekly_subset)

df_cnt_weekly <- df_cnt_weekly_subset
rm(df_cnt_weekly_subset)

# change Object_ID_cnt to factors
# do it after removing certain counties to avoid having factors with zero counts
# (this messes up the cross-basis function)
df_cnt_weekly$Object_ID_cnt <- as.factor(df_cnt_weekly$Object_ID_cnt)

# check how many counties
length(levels(df_cnt_weekly$Object_ID_cnt))


#'
#' ## refine in time : limit to months of high peak ##
#'

# Limit to August-September-October
df_cnt_weekly_highMonths <- subset(df_cnt_weekly, m %in% 8:10)
# Do not subset df_cnt_weekly here because will need lags later
# The subset will be done when calling the regression.

sum(df_cnt_weekly_highMonths$count==0)/nrow(df_cnt_weekly_highMonths)


#' 
#' ## this is the final histogram of counts per county-week ##
#' 

# the following code is a bit annoying 
# but looks like it's what it takes to get a proper histogram
# with broken y axis

library(plotrix)

# this is the subset
df_cnt_weekly_subset <-
  subset(df_cnt_weekly, m %in% 8:10)

# count how many counts per county-week
tab_countsPerCntWk <- table(df_cnt_weekly_subset$count)
df_countsPerCntWk <- data.frame(
  Var1 = as.integer(names(tab_countsPerCntWk)),
  Freq = as.integer(tab_countsPerCntWk))
# missing counts with zero county-week 
df_countsPerCntWk <-
  dplyr::left_join(
    x = data.frame(Var1=0:max(df_countsPerCntWk$Var1)),
    y = df_countsPerCntWk,
    by.id="Var1")
# replace NAs by zeros
df_countsPerCntWk$Freq[which(is.na(df_countsPerCntWk$Freq))] <- 0
# back to table for gap.barplot
tab_countsPerCntWk <- df_countsPerCntWk$Freq
names(tab_countsPerCntWk) <- as.character(df_countsPerCntWk$Var1)

tab_countsPerCntWk

# make the plot (finally)
ylocs = c(seq(from=0,to=450,by=50),9408)
gap.barplot(tab_countsPerCntWk,
            gap=c(500,9300),
            xaxlab = names(tab_countsPerCntWk),
            ytics = ylocs,yaxlab = ylocs,
            xlab="leptospirosis counts in county-week",
            ylab="counts of county-week")


#'
#' # Create basis vectors #
#'


# this is for the long-term temporal trend
# ob.trend <- onebasis(x = df_cnt_weekly$y,
#                      fun = 'strata',
#                      breaks=2003:2014)
# idx=sample(x = 1:nrow(df_cnt_weekly),size = 100);
# plot(x = df_cnt_weekly[idx,]$time, y = ob.trend[idx,1])
# plot(x = df_cnt_weekly[idx,]$time, y = ob.trend[idx,6])

# ob.seasonal <- onebasis(x = df_cnt_weekly$woy,
#                         fun = 'ns', # phil had pbs: why?
#                         Boundary.knots=c(30,44),
#                         df=3)

# define crossbasis for rainfall
cb.r <- crossbasis(
  x = df_cnt_weekly$r,
  lag = 15, # in weeks
  argvar = list(fun="poly",deg=1),     
  arglag = list(fun='ns',df=3),knots=c(1,5,10),
  group = df_cnt_weekly$Object_ID_cnt)

# ob.r <- onebasis(x = df_cnt_weekly$r,
#                  fun = "lin")


# define basis for temperature 

# ob.t <- onebasis(x = df_cnt_weekly$t,
#                  fun = "lin")

# define crossbasis for temperature
cb.t <- crossbasis(
  x = df_cnt_weekly$t,
  lag = 15,
  argvar = list(fun="poly",deg=1),     
  # argvar = list(fun='strata',breaks=13),
  arglag = list(fun='ns',df=3),
  group = df_cnt_weekly$Object_ID_cnt)

#'
#' ## refine to temporal subset ##
#'

# define indices to be applied to crossbases
row_keep <-
  which(df_cnt_weekly$m %in% 8:10)

# take subset for epi data
df_cnt_weekly_subset <- df_cnt_weekly[row_keep,]

# take subset for seasonal
# ob.seasonal_subset <- ob.seasonal[row_keep,]
# attributes_ob.seasonal_subset <- attributes(ob.seasonal)
# attributes_ob.seasonal_subset$dim <- dim(ob.seasonal_subset)
# attributes(ob.seasonal_subset) <- attributes_ob.seasonal_subset

# take subset for environmental crossbases
cb.r_subset <- cb.r[row_keep,]
# but problem : this converts crosspred to an object of type matrix
# however crosspred object is needed for predictions
# lines below are a dirty way of avoiding that
attributes_cb.r_subset <- attributes(cb.r)
attributes_cb.r_subset$dim <- dim(cb.r_subset)
attributes(cb.r_subset) <- attributes_cb.r_subset

# same for temperature cross basis
cb.t_subset <- cb.t[row_keep,]
# but problem : this converts crosspred to an object of type matrix
# however crosspred object is needed for predictions
# lines below are a dirty way of avoiding that
attributes_cb.t_subset <- attributes(cb.t)
attributes_cb.t_subset$dim <- dim(cb.t_subset)
attributes(cb.t_subset) <- attributes_cb.t_subset

#'
#' # Call model #
#'

model.rt <- 
  glm(count ~ 
        Object_ID_cnt + cb.r_subset + cb.t_subset + offset(total_scaled),
      data = df_cnt_weekly_subset,
      family = quasipoisson(link = 'log'))

#'
#' #  Plot all parts of the prediction model #
#'  

# time trend
# 
# pred.r_trend <- crosspred(basis = ob.trend,
#                           model = model.rt)
# 
# plot(pred.r_trend)


# seasonal part

# pred.rt_seasonal <- crosspred(basis = ob.seasonal,
#                               model = model.rt,
#                               at = pretty(df_cnt_weekly_subset$woy),
#                               cen = 0)
# plot(pred.rt_seasonal)


#'
#' ## rainfall component ##
#'  

pred.rt_r <- crosspred(basis = cb.r_subset,
                       model = model.rt,
                       at = pretty(df_cnt_weekly_subset$r,n = 100),
                       cen = 0)

plot(pred.rt_r)

plot(pred.rt_r,"contour",
     plot.title=title(xlab="daily rainfall [mm]",
                      ylab="lag [weeks]"),
     key.title=title("relative\nrisk"))

plot(pred.rt_r,"slices",var=c(0,10,20),lag=c(0,8,15),
     ci.level=0.90,ci.arg=list(density=20,col=grey(0.7)))


plot(pred.rt_r,'slices',lag=0)
plot(pred.rt_r,'slices',lag=8)

plot(pred.rt_r,'slices',var=20)



# plot overall effect

plot(pred.rt_r,"overall",ci="lines",main="overall effect")


#'
#' ## temperature component ##
#' 


pred.rt_t <- crosspred(basis = cb.t,
                       model = model.rt,
                       at = pretty(df_cnt_weekly_subset$t,n = 100),
                       # keep cen at 0 to have always low reference
                       cen = 0) 

plot(pred.rt_t)

plot(pred.rt_t,"contour",
     plot.title=title(xlab="temperature [C]",
                      ylab="lag [weeks]"),
     key.title=title("relative\nrisk"))

# plot(pred.rt_t,"slices",var=c(11,20,30),lag=c(0,8,15),
#      ci.level=0.90,ci.arg=list(density=20,col=grey(0.7)))


plot(pred.rt_t,'slices',lag=0)
plot(pred.rt_t,'slices',var=20)

plot(pred.rt_t,'slices',lag=9)

# plot overall effect

# plot(pred.rt_t,"overall",ci="lines",main="overall effect")

