#' ---
#' title: "Plot time series of vic+lep data at weekly/yearly resolution"
#' author: "Karina Cucchi"
#' date: "December 4th, 2017"
#' ---
#' 

library(ggplot2)
library(lubridate)

#'
#' # load data #
#'

#' load lepto data 
df_year_lep <- 
  readRDS('../../../data/2_formatted_data/df_year_lep.rds')
df_year_lep

df_week_lep <- 
  readRDS('../../../data/2_formatted_data/df_week_lep.rds')

# get rid of years previous to 2004
df_year_lep <- 
  subset(x = df_year_lep,subset = as.numeric(year) >= 2004)
df_week_lep <- 
  subset(x = df_week_lep,
         subset = lubridate::year(as.Date(x = df_week_lep$sundays)) >= 2004)
str(df_week_lep)

#' load vic data
df_year_vic <- 
  readRDS('../../../data/2_formatted_data/df_year_vic.rds')
df_week_vic <-
  readRDS('../../../data/2_formatted_data/df_week_vic.rds')



# names_vic <-
  # c("vic_Prcp","vic_sm1","vic_roff")
names_vic <- c("vic_Tmin","vic_Tmax")

#'
#' # Yearly #
#'

#'
#' ## format and plot time series ##
#'

df_year <- 
  dplyr::left_join(
    x=df_year_lep[,c('year','count')],
    y=df_year_vic[,c("year",names_vic)],
    by="year")

df_year_long <-
  reshape2::melt(data = df_year,id="year")

g_year_vic <-
  ggplot(data = df_year_long) +
  geom_line(mapping = aes(x=year,y=value,
                          group=variable,col=variable)) +
  facet_wrap(~ variable,scales = "free_y",ncol=1) +
  theme_bw() +
  theme(legend.position = "none")

g_year_vic

pdf(file = '../g_vic_year_hydro.pdf',width = 4,height=5)
g_year_vic
dev.off()


#'
#' ## also check scatterplots ##
#'

list_g_scatter_year <- list()
for(i_vic in 1:length(names_vic)){
  list_g_scatter_year[[i_vic]] <-
    ggplot(data = df_year) +
    geom_point(mapping = aes_string(x=names_vic[i_vic],
                                    y="count",
                                    col="year")) +
    geom_smooth(mapping = aes_string(x=names_vic[i_vic],
                                     y="count"),
                method = "lm") +
    theme_bw()
}

g_year_scatter_vic <-
  cowplot::plot_grid(plotlist = list_g_scatter_year,ncol=1)

g_year_scatter_vic

pdf(file = '../g_vic_year_scatter_hydro.pdf',width=4,height=7)
g_year_scatter_vic
dev.off()

#'
#' ## perform correlations ##
#'



#'
#' # Weekly #
#'

#'
#' ## format and plot time series ##
#'

df_week <- 
  dplyr::left_join(
    x=df_week_lep[,c('sundays','count')],
    y=df_week_vic[,c("sundays",names_vic)],
    by="sundays")

df_week_long <-
  reshape2::melt(data = df_week,id="sundays")

df_week_long$sundays_date <- 
  as.Date(df_week_long$sundays)

g_week_vic <-
  ggplot(data = df_week_long) +
  geom_line(mapping = aes(x=sundays_date,y=value,
                          group=variable,col=variable)) +
  scale_x_date(name = "",date_breaks = "1 year",date_labels = "%Y") +
  facet_wrap(~ variable,scales = "free_y",ncol=1) +
  theme_bw() +
  theme(legend.position = "none")

g_week_vic

pdf(file = '../g_vic_week_temperature.pdf',width = 4,height=6)
g_week_vic
dev.off()


#'
#' ## also check scatterplots ##
#'

list_g_scatter_week <- list()
for(i_vic in 1:length(names_vic)){
  list_g_scatter_week[[i_vic]] <-
    ggplot(data = df_week) +
    geom_point(mapping = aes_string(x=names_vic[i_vic],
                                    y="count")) +
    geom_smooth(mapping = aes_string(x=names_vic[i_vic],
                                     y="count"),
                method = "lm") +
    theme_bw()
}

g_year_scatter_vic <-
  cowplot::plot_grid(plotlist = list_g_scatter_week,ncol=1)

g_year_scatter_vic

# dont mean anything at the weekly timescale so I don't plot them

#'
#' ## perform correlations ##
#'

# get rid of na values (not accepted by cross-correlation function)
df_week <- 
  subset(x = df_week,
         subset = !is.na(df_week[,names_vic[1]]))

# initialize dataframe with results
names_df_ccf <- c("var","lag","ccf")
df_ccf_week <- 
  data.frame(matrix(data = NA,
                    nrow = 0,ncol = length(names_df_ccf)))
names(df_ccf_week) <- names_df_ccf

# loop over vic variables
i_vic <- 1
for(i_vic in 1:length(names_vic)){
  
  ccf_weekly <- 
    ccf(x = as.ts(df_week$count),
        y = as.ts(df_week[,names_vic[i_vic]]),
        lag.max = 50,plot = F)
  
  df_ccf_week <-
    rbind(df_ccf_week,
          data.frame(var=names_vic[i_vic],
                     lag=ccf_weekly$lag[,1,1],
                     ccf=ccf_weekly$acf[,1,1]))
}

# calculate significant correlation level according to 
# https://stats.stackexchange.com/questions/3115/cross-correlation-significance-in-r
sign_level <- 2/sqrt(nrow(df_week))

g_ccf_weekly <-
  ggplot(data = df_ccf_week,
         mapping = aes(x=lag,y=ccf,fill=var,col=var)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,25,5),
                     minor_breaks = 0:25,
                     limits = c(0,25)) +
  geom_hline(yintercept = sign_level,color="grey50",linetype=2) +
  geom_hline(yintercept = 0,color="grey50") +
  labs(x=expression("lag "*tau*" (in weeks)"),
       y=paste0("cross-correlation"),
       col="variable") +
  theme_bw() +
  theme(legend.position = "top")

g_ccf_weekly

pdf(file = '../g_vic_weekly_ccf_temperature.pdf',width=4,height=4)
g_ccf_weekly
dev.off()

