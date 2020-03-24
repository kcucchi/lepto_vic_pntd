
library(splines)
library(pbs)
library(dlnm)
library(ggplot2)

# contains code for the definition of constrained splines 
# for time spline each year
source('../../../R_utils/cobs_perYear.R')

#'
#' # Get and format data #
#'

#'
#' ## get data ##
#'

# get data at weekly and county resolution
df_cnt_week <- 
  readRDS('../../../data/2_formatted_data/df_cnt_week_final.rds')

# limit to years 2004-2014
# (years of lepto surveillance)

df_cnt_week <- 
  subset(x = df_cnt_week,y >= 2004)

head(df_cnt_week)

#'
#' ## refine by occupation : limit to farmers ##
#'

df_cnt_week$count <- df_cnt_week$count_farmers
head(df_cnt_week)

#'
#' ## refine in space ##
#'

df_cnt_week <- subset(df_cnt_week,cntWithLep_farmers)

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

df_cnt_week_subset$CNTY_CODE <- as.factor(df_cnt_week_subset$CNTY_CODE)

# check how many counties
length(levels(df_cnt_week_subset$CNTY_CODE))


#' 
#' ## this is the final histogram of counts per county-week ##
#' 

# the following code is a bit annoying 
# but looks like it's what it takes to get a proper histogram
# with broken y axis

library(plotrix)

# count how many counts per county-week
tab_countsPerCntWk <- table(df_cnt_week_subset$count)
df_countsPerCntWk <- data.frame(
  Var1 = as.integer(names(tab_countsPerCntWk)),
  Freq = as.integer(tab_countsPerCntWk))
# missing counts with zero county-week 
df_countsPerCntWk <-
  dplyr::left_join(
    x = data.frame(Var1=0:max(df_countsPerCntWk$Var1)),
    y = df_countsPerCntWk,
    by="Var1")
# replace NAs by zeros
df_countsPerCntWk$Freq[which(is.na(df_countsPerCntWk$Freq))] <- 0
# back to table for gap.barplot
tab_countsPerCntWk <- df_countsPerCntWk$Freq
names(tab_countsPerCntWk) <- as.character(df_countsPerCntWk$Var1)

tab_countsPerCntWk
# barplot(tab_countsPerCntWk)
# make the plot (finally)
ylocs = c(pretty(tab_countsPerCntWk[as.character(1:length(tab_countsPerCntWk))]),
          tab_countsPerCntWk['0'])
gap.barplot(tab_countsPerCntWk,
            gap=c(ylocs[length(ylocs)-1],
                  (2*ylocs[2])*(floor(tab_countsPerCntWk['0']/(2*ylocs[2])))),
            xaxlab = names(tab_countsPerCntWk),
            ytics = ylocs,yaxlab = ylocs,
            xlab="leptospirosis counts in county-week",
            ylab="counts of county-week")


#'
#' # Create basis vectors #
#'

#'
#' ## Long-term temporal trend ##
#'

# this is for the long-term temporal trend

# # use cobs_perYear function to start and finsh at zeros every year
ob.trend <- cobs_perYear(df_week_year = df_cnt_week_subset,dfPerYear = 2)
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
#' ## Rainfall cross-basis ##
#'

# define crossbasis for rainfall
# do each time step separately
cb.r <- crossbasis(
  x = df_cnt_week$cmorph_mmPday,
  lag = 15, # in weeks
  argvar = list(fun="poly",deg=1),     
  arglag = list(fun="integer"), #unconstrained
  group = df_cnt_week$CNTY_CODE)

#'
#' ## Temperature cross-basis ##
#'

# define crossbasis for temperature
cb.t <- crossbasis(
  x = df_cnt_week$temp_C,
  lag = 15,
  argvar = list(fun="poly",deg=1),     
  # argvar = list(fun='strata',breaks=13),
  arglag = list(fun="integer"), #unconstrained
  group = df_cnt_week$CNTY_CODE)

#'
#' ## Refine cross bases to temporal subset ##
#'

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
#' ## define interquartile ranges for rainfall and temperature ##
#'

cb.r_subset_scaled <- 
  attributes(cb.r_subset)$range[1] + 
  diff(attributes(cb.r_subset)$range) * cb.r_subset

qnt_r <- as.numeric(quantile(cb.r_subset_scaled,probs=c(0.25,0.75)))

qnt_r

cb.t_subset_scaled <- 
  attributes(cb.t_subset)$range[1] + 
  diff(attributes(cb.t_subset)$range) * cb.t_subset

qnt_t <- as.numeric(quantile(cb.t_subset_scaled,probs=c(0.25,0.75),na.rm = T))

qnt_t

#'
#' # Call model #
#'

# introduce counties as dummy variables
# indicator variable for each county (factors are used as dummy variables)

model.rt <- 
  glm(count ~ 
        ob.trend +
        cb.r_subset + cb.t_subset +
        offset(log(pop_agr)) + 
        CNTY_CODE,
      data = df_cnt_week_subset,
      family = quasipoisson(link = 'log'))

summary(model.rt)

#'
#' #  Plot all parts of the prediction model #
#'  

#'
#' ## time trend ##
#' 

# # get coefficients from fit
time_coefs <-
  model.rt$coefficients[grep(pattern = "ob.trend",
                             x = names(model.rt$coefficients))]

# do the fit
time_fit <- rowSums(ob.trend[,which(!is.na(time_coefs))] %*%
                      diag(time_coefs[which(!is.na(time_coefs))]))


df_time <- data.frame(sundays_date=df_cnt_week_subset$sundays_date,
                      time=df_cnt_week_subset$time,
                      fit=time_fit)

df_time <- df_time[!duplicated(df_time), ]

ggplot(df_time) +
  geom_line(aes(x=sundays_date,y=fit)) +
  xlab('date') + ylab('fitted constrained time splines')

# seasonal part

# pred.rt_seasonal <- crosspred(basis = ob.seasonal,
#                               model = model.rt,
#                               at = pretty(df_cnt_week_subset$woy),
#                               cen = 0)
# plot(pred.rt_seasonal)


#'
#' ## rainfall component ##
#'  

pred.rt_r <- crosspred(basis = cb.r_subset,
                       model = model.rt,
                       at = pretty(df_cnt_week_subset$r,n = 100),
                       cen = qnt_r[1]) # set reference at 1st rainfall quartile

# plot(pred.rt_r)
# 
# plot(pred.rt_r,"contour",
#      plot.title=title(xlab="daily rainfall [mm]",
#                       ylab="lag [weeks]"),
#      key.title=title("relative\nrisk"))
# 
# plot(pred.rt_r,"slices",var=c(0,10,20),lag=c(0,8,15),
#      ci.level=0.95,ci.arg=list(density=20,col=grey(0.7)))
# 
# 
# plot(pred.rt_r,'slices',lag=0)
# plot(pred.rt_r,'slices',lag=8)
# 
# plot(pred.rt_r,'slices',var=20)
# 
# 
# # plot overall effect
# 
# plot(pred.rt_r,"overall",ci="lines",main="overall effect")


#'
#' ## temperature component ##
#' 

#'
#' The increase in relative risk with respect to temperature is evaluated
#' looking at interquartile ranges.
#'

pred.rt_t <- crosspred(basis = cb.t_subset,
                       model = model.rt,
                       at = pretty(df_cnt_week_subset$t,n = 100),
                       cen = qnt_t[1])  # set reference at 1st temperature quartile

# plot(pred.rt_t)
# 
# plot(pred.rt_t,"contour",
#      plot.title=title(xlab="temperature [C]",
#                       ylab="lag [weeks]"),
#      key.title=title("relative\nrisk"))
# 
# # plot(pred.rt_t,"slices",var=c(11,20,30),lag=c(0,8,15),
# #      ci.level=0.90,ci.arg=list(density=20,col=grey(0.7)))
# 
# 
# plot(pred.rt_t,'slices',lag=0)
# plot(pred.rt_t,'slices',var=qnt_t[2])
# plot(pred.rt_t,'slices',var=qnt_t[2],ylim=c(0,10))
# 
# plot(pred.rt_t,'slices',lag=8)

# plot overall effect

# plot(pred.rt_t,"overall",ci="lines",main="overall effect")

#'
#' # make ggplots #
#'

library(cowplot)

# value for which to plot the relative risks
r_val=qnt_r[2] 
# find closest value in vector of predicted values
idx_val <- 
  which(abs(pred.rt_r$predvar-r_val)==min(abs(pred.rt_r$predvar-r_val)))
# get lag values
df_plot <- data.frame(lag=seq(from=pred.rt_r$lag[1],
                              to=pred.rt_r$lag[2],
                              by=pred.rt_r$bylag))
df_plot$fit <- pred.rt_r$matRRfit[idx_val,]
df_plot$low <- pred.rt_r$matRRlow[idx_val,] 
df_plot$high <- pred.rt_r$matRRhigh[idx_val,]

df_plot_long <- reshape2::melt(df_plot,id="lag")

g_rain <-
  ggplot(df_plot_long) +
  geom_line(aes(x=lag,y=value,color=variable)) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = c(0,5,10,15),minor_breaks = 1:14) +
  scale_colour_manual(values=c(fit='red',low='grey30',high='grey30')) +
  theme_minimal() +
  labs(x="lags (weeks)",y="RR") +
  theme(legend.position="none")

g_rain_eb <-
  ggplot(df_plot,aes(x=lag,y=fit)) +
  geom_errorbar(aes(ymin=low, ymax=high),
                width=.1,colour="grey50") +
  geom_point(shape=2,size=3,col=rev(RColorBrewer::brewer.pal(n = 3, name = "RdBu"))[1]) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = c(0,5,10,15),minor_breaks = 1:14) +
  theme_minimal() +
  labs(x="lags (weeks)",y="RR") +
  theme(legend.position="none")

g_rain_eb


# value for which to plot the relative risks
t_val=qnt_t[2] 
# find closest value in vector of predicted values
idx_val <- 
  which(abs(pred.rt_t$predvar-t_val)==min(abs(pred.rt_t$predvar-t_val)))
# get lag values
df_plot <- data.frame(lag=seq(from=pred.rt_t$lag[1],
                              to=pred.rt_t$lag[2],
                              by=pred.rt_t$bylag))
df_plot$fit <- pred.rt_t$matRRfit[idx_val,]
df_plot$low <- pred.rt_t$matRRlow[idx_val,] 
df_plot$high <- pred.rt_t$matRRhigh[idx_val,]

df_plot_long <- reshape2::melt(df_plot,id="lag")


g_t <-
  ggplot(df_plot_long) +
  geom_line(aes(x=lag,y=value,color=variable)) +
  geom_hline(yintercept = 1) +
  scale_colour_manual(values=c(fit='red',low='grey30',high='grey30')) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,5,10,15),minor_breaks = 1:14) +
  # scale_y_continuous(breaks = 10^pretty(log10(df_plot_long$value))) +
  labs(x="lags (weeks)",y="RR") +
  # coord_trans(y = "log10") + 
  theme(legend.position="none")

g_t

g_t_eb <-
  ggplot(df_plot,aes(x=lag,y=fit)) +
  geom_errorbar(aes(ymin=low, ymax=high),
                width=.1,colour="grey50") +
  geom_point(shape=2,size=3,col=rev(RColorBrewer::brewer.pal(n = 3, name = "RdBu"))[3]) +
  geom_hline(yintercept = 1) +
  scale_x_continuous(breaks = c(0,5,10,15),minor_breaks = 1:14) +
  theme_minimal() +
  labs(x="lags (weeks)",y="RR") +
  theme(legend.position="none")

g_t_eb

dev.off()
pdf('final_plot.pdf',width=8,height=4)
cowplot::plot_grid(g_rain_eb, g_t_eb, 
                   labels = c('rainfall',
                              'temperature'))
dev.off()


#'
#' ## what am I trying to fit with temperature - scatterplots ##
#'

# create dataframe with counts and lagged temperatures
t_min <- attr(cb.t_subset,"range")[1]
t_max <- attr(cb.t_subset,"range")[2]
df_t_lags <- as.data.frame(t_min + (t_max - t_min) * cb.t_subset)
# change names to values of lags (in weeks)
names(df_t_lags) <- paste0("lag_",attr(cb.t_subset,"arglag")$values,"_weeks")
# add lepto counts
df_t_lags$log_count <- log(df_cnt_week_subset$count+1)
# melt dataframe
df_t_lags_long <- reshape2::melt(df_t_lags,id="log_count")
# rename columns
names(df_t_lags_long) <- c("log_count","lag","temperature")
str(df_t_lags_long)

pdf('final_plot_tfacets.pdf',width=7,height=6)
ggplot(df_t_lags_long, aes(x=temperature, y=log_count)) +
  geom_point(shape=1,alpha=0.1) +
  facet_wrap( ~ lag,ncol=4) +
  geom_vline(xintercept = 20,col="red",linetype="dotted")+
  ylab("log(count+1)") + xlab("temperature [C]")
dev.off()

# Remarks:
# (1) I don't see a particular reason why week 8 should have different risk than other weeks
# (2) for lags between 7 and 12 weeks, no lepto counts below 20C...
# it seems that it is the combination of rainfall + high enough temperatures 
# that increase lepto counts...
# this would have to do with the survival of leptospira in the environment

#'
#' ## compare density of temperatures at 8-weeks lag for counties-weeks with/without counts ##
#'

df_t_lags$count <- df_cnt_week_subset$count
df_t_lags$isNonZeroCount <- as.factor(as.numeric(df_cnt_week_subset$count != 0))

df_t_lags_long <- 
  reshape2::melt(df_t_lags[,setdiff(names(df_t_lags),c("count","log_count"))],
                 id="isNonZeroCount")
names(df_t_lags_long) <- c("isNonZeroCount","lag","temperature")

str(df_t_lags_long)

pdf('final_plot_tDens.pdf',width=9,height=6)
ggplot(df_t_lags_long, aes(x=temperature, fill = isNonZeroCount)) +
  stat_density(aes(y = ..density..), 
               position = "identity",
               color = "black", alpha = 0.5) +
  facet_wrap( ~ lag,ncol=4) +
  geom_vline(xintercept = 20,col="red",linetype="dotted") +
  xlab("temperature [C]") +
  scale_fill_manual(values=c("0"="blue", "1"="red"),
                    labels = c("county-week without cases",
                               "county-week with cases")) +
  guides(fill=guide_legend(title=""))
dev.off()







