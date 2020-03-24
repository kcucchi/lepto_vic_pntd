#' ---
#' title: "Create dataframe with all variables at county-weekly resolution"
#' author: "Karina Cucchi"
#' date: "September 21st, 2017"
#' ---
#' 


library(lubridate)
library(dplyr)
library(reshape2)


#' I want to create a dataframe where each line correponds to:
#' * a given county
#' * a given week
#' * population in county
#' * whether it is in a prefecture with cases
#' * whether the county contains at least one case
#' * number of lepto cases
#' * mean rainfall in week
#' * mean temperature in week
#' 

#' # Load weekly/county data #

#' ## load reference county-weekly dataframe ##

df_cnt_week <- readRDS('../df_cnt_week_ref.rds')

#' ## load weekly/county population data ##

df_pop <- readRDS('../df_cnt_week_pop.rds')
str(df_pop)

#' ## load weekly/county health data ##

df_lep <- readRDS('../df_cnt_week_lep.rds')
str(df_lep)
# Each row is a county/week
# CNTY_CODE is the corresponding county idx
# sundays is the date of the preceding sunday
# count is human lepto count
# count_farmers is human lepto count restricted to farmers
# NAs before 2003 because surveillance started in 2003

#'
#' ## load cnt-level statistics ##
#'

df_cnt_lep <- readRDS('../df_cnt_lep.rds')
str(df_cnt_lep)

#' 
#' ## load weekly/county rainfall data ##
#' 

df_cmorph <- readRDS('../df_cnt_week_cmorph.rds')
str(df_cmorph)
# Each row is a county/week
# CNTY_CODE is the corresponding county idx
# sundays is the date of the preceding sunday
# cmorph_mmPday is the average daily rainfall


#'
#' ## load weekly/county temperature data ##
#'

df_temp <- readRDS('../df_cnt_week_berkeleyearth.rds')
str(df_temp)
# Each row is a county/week
# CNTY_CODE is the corresponding county idx
# sundays is the date of the preceding sunday
# temp_C is the average temperature

#'
#' ## load weekly/county vic data ##
#'

df_vic <- readRDS('../df_cnt_week_vic.rds')
str(df_vic)

#'
#'  # Merge data #
#'  

# add population
df_cnt_week <- dplyr::left_join(x = df_cnt_week,
                                y = df_pop,
                                by=c("sundays","CNTY_CODE"))


# add lep counts
df_cnt_week <- dplyr::left_join(x = df_cnt_week,
                                y = df_lep,
                                by=c("sundays","CNTY_CODE"))

# add county-level lep statistics
df_cnt_week <- dplyr::left_join(x = df_cnt_week,
                                y = df_cnt_lep[,setdiff(names(df_cnt_lep),
                                                        c("count","count_farmers"))],
                                by = c("CNTY_CODE"))

# add rainfall
df_cnt_week <- dplyr::left_join(x = df_cnt_week,
                                y = df_cmorph,
                                by=c("sundays","CNTY_CODE"))

# add temperature
df_cnt_week <- dplyr::left_join(x = df_cnt_week,
                                y = df_temp,
                                by=c("sundays","CNTY_CODE"))

str(df_cnt_week)

# add vic
df_cnt_week <- dplyr::left_join(x = df_cnt_week,
                                y = df_vic,
                                by=c("sundays","CNTY_CODE"))

str(df_cnt_week)


#'
#' # Add additional variables for time series analysis #
#'


df_cnt_week$sundays_date <- as.Date(df_cnt_week$sundays)

# define field for week in year
df_cnt_week$woy <-
  as.integer(format(df_cnt_week$sundays_date,
                    format="%U"))

# define field for week since first date
df_cnt_week$time <-
  as.integer(difftime(time1 = df_cnt_week$sundays_date,
                      time2 = min(df_cnt_week$sundays_date),
                      units = "week"))

# define field for year
df_cnt_week$y <- 
  lubridate::year(df_cnt_week$sundays_date)

# define field for month
df_cnt_week$m <- 
  lubridate::month(df_cnt_week$sundays_date)

head(df_cnt_week)

#'
#' # Save final variable #
#'

saveRDS(object = df_cnt_week,file = '../df_cnt_week_final.rds')



#' 
#' #'
#' #' # check statistics of weekly rainfall #
#' #'
#' 
#' prob_vect=c(0.5,0.75,0.9,0.95,0.99,0.999,0.9999)
#' quant_vect <- quantile(x = df_cnt_weekly$r,probs = prob_vect)
#' quant_vect
#' 
#' if(plotPdf) pdf('cnt_r_ecdf.pdf',width=4,height=4)
#' plot(ecdf(df_cnt_weekly$r),
#'      main="",
#'      xlab="rainfall [mm]",
#'      ylab="empirical CDF")
#' abline(h=prob_vect,col="red",lty=2)
#' abline(v=quant_vect,col="red",lty=2)
#' text(x=quant_vect,y=c(0,0.05),labels = prob_vect,
#'      col='red',cex = 0.7)
#' if(plotPdf) dev.off()
#' 
#' 
#' # number of county-weeks above 30mm
#' sum(df_cnt_weekly$r >= 30)
#' # percentage
#' sum(df_cnt_weekly$r >= 30)/nrow(df_cnt_weekly)
#' 
#' # number of county-weeks above 50mm
#' sum(df_cnt_weekly$r >= 50)
#' # percentage
#' sum(df_cnt_weekly$r >= 50)/nrow(df_cnt_weekly)
#' 
#' #'
#' #' # check statistics of counts #
#' #'
#' 
#' # spatial:
#' # (1) county with pref with at least one case
#' # (2) county with at least one case
#' # (3) 20 counties with most population
#' # (4) 20 counties with most lep cases
#' 
#' # temporal:
#' # (1) all year
#' # (2) Aug-Oct
#' 
#' stat_counts <-
#'   expand.grid(spatial=c('prefectureWithLep',
#'                         'countyWithLep',
#'                         'popThreshold',
#'                         'lepThreshold_3',
#'                         'lepThreshold_10',
#'                         'lepThreshold_20'),
#'               temporal=c('all','Aug_Oct'),stringsAsFactors = F)
#' 
#' stat_counts$perc_zeros <- rep(NA,nrow(stat_counts))
#' stat_counts$mean <- rep(NA,nrow(stat_counts))
#' stat_counts$var <- rep(NA,nrow(stat_counts))
#' # statistics of log-transformed counts
#' stat_counts$mean_log <- rep(NA,nrow(stat_counts))
#' stat_counts$var_log <- rep(NA,nrow(stat_counts))
#' 
#' # consider only subset with available population data
#' df_cnt_weekly <- subset(df_cnt_weekly,!missing_twn_pop)
#' 
#' # function to fill in stat_counts
#' fill_zerosStat <- function(stat_counts, df_subset,nameSpatial,nameTemporal){
#'   idx_row <- which(stat_counts$spatial== nameSpatial&
#'                      stat_counts$temporal==nameTemporal)
#'   stat_counts[idx_row,'perc_zeros'] <- 
#'     100 * sum(df_subset$count == 0) / nrow(df_subset)
#'   stat_counts[idx_row,'mean'] <- mean(df_subset$count)
#'   stat_counts[idx_row,'var'] <- var(df_subset$count)
#'   stat_counts[idx_row,'mean_log'] <- mean(log10(df_subset$count+1))
#'   stat_counts[idx_row,'var_log'] <- var(log10(df_subset$count+1))
#'   return(stat_counts)
#' }
#' 
#' ## counties within prefectures with at least one case ##
#' 
#' # all time
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = prefectureWithLep),
#'                  nameSpatial='prefectureWithLep',
#'                  nameTemporal = 'all')
#' 
#' # Aug-Oct
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = prefectureWithLep & 
#'                                       (m %in% 8:10)),
#'                  nameSpatial='prefectureWithLep',
#'                  nameTemporal = 'Aug_Oct')
#' 
#' ## counties within counties with at least one case ##
#' 
#' # all time
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = countyWithLep),
#'                  nameSpatial='countyWithLep',
#'                  nameTemporal = 'all')
#' 
#' # Aug-Oct
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = countyWithLep & 
#'                                       (m %in% 8:10)),
#'                  nameSpatial='countyWithLep',
#'                  nameTemporal = 'Aug_Oct')
#' 
#' ## select 20 most populated counties ##
#' 
#' # all time
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byPop <= 20),
#'                  nameSpatial='popThreshold',
#'                  nameTemporal = 'all')
#' 
#' # Aug-Oct
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byPop <= 20 & 
#'                                       (m %in% 8:10)),
#'                  nameSpatial='popThreshold',
#'                  nameTemporal = 'Aug_Oct')
#' 
#' ## select 3 counties with most lep cases ##
#' 
#' # all time
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byLep <= 3),
#'                  nameSpatial='lepThreshold_3',
#'                  nameTemporal = 'all')
#' 
#' # Aug-Oct
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byLep <= 3 & 
#'                                       (m %in% 8:10)),
#'                  nameSpatial='lepThreshold_3',
#'                  nameTemporal = 'Aug_Oct')
#' 
#' ## select 10 counties with most lep cases ##
#' 
#' # all time
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byLep <= 10),
#'                  nameSpatial='lepThreshold_10',
#'                  nameTemporal = 'all')
#' 
#' # Aug-Oct
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byLep <= 10 & 
#'                                       (m %in% 8:10)),
#'                  nameSpatial='lepThreshold_10',
#'                  nameTemporal = 'Aug_Oct')
#' 
#' ## select 20 counties with most lep cases ##
#' 
#' # all time
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byLep <= 20),
#'                  nameSpatial='lepThreshold_20',
#'                  nameTemporal = 'all')
#' 
#' # Aug-Oct
#' stat_counts <- 
#'   fill_zerosStat(stat_counts = stat_counts,
#'                  df_subset = subset(x = df_cnt_weekly,
#'                                     subset = rank_byLep <= 20 & 
#'                                       (m %in% 8:10)),
#'                  nameSpatial='lepThreshold_20',
#'                  nameTemporal = 'Aug_Oct')
#' 
#' library(ggplot2)
#' # plot percentage of zeros
#' gPlot_statCounts <-
#'   ggplot(data=stat_counts,aes(x=spatial,y=perc_zeros,col=temporal)) +
#'   geom_point() +
#'   ylim(0,100)+
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1))
#' gPlot_statCounts
#' 
#' 
#' 
#' gPlot_meanVar <-
#'   ggplot(data=stat_counts,aes(x=spatial,y=var/mean,
#'                              col=temporal)) +
#'   geom_point() +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1))
#' gPlot_meanVar
#' 
#' gPlot_meanVarLog <-
#'   ggplot(data=stat_counts,aes(x=spatial,y=var_log/mean_log,
#'                               col=temporal)) +
#'   geom_point() +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#'   ylim(0.5,1)
#' gPlot_meanVarLog
#' 
#' #' Actually it seems that when looking at log of counts 
#' #' the data is not overdispersed...