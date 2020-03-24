#' ---
#' title: "Define reference county-year and county-week dataframes"
#' author: "Karina Cucchi"
#' date: "September 13th, 2017"
#' ---
#' 


library(sp)
library(dplyr)

#'
#' # Load data #
#' 

# load county dataset
counties <- 
  readRDS(file = '../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')


#'
#' ## define reference for year and county-year ##
#'

# define initial dataframe for years
df_year <- data.frame(year=as.character(2003:2014),stringsAsFactors = F)
str(df_year)

saveRDS(object = df_year,
        file = '../df_year_ref.rds')

# expand with codes for counties
df_cnt_year <-
  expand.grid(year=df_year$year,
              GBcode_cnt=as.character(counties$GBcode_cnt),
              stringsAsFactors = F)
str(df_cnt_year)

# save 
saveRDS(object = df_cnt_year,
        file = '../df_cnt_year_ref.rds')

#'
#' ## define reference for week and county-week ##
#'

# define initial dataframe for weeks
df_week <- data.frame(
  sundays=
    format(seq.Date(from=lubridate::floor_date(x = as.Date('2003-01-01'),
                                               unit = "weeks"),
                    to=lubridate::floor_date(x = as.Date('2014-12-31'),
                                             unit = "weeks"),
                    by="week")),
  stringsAsFactors = F
)

str(df_week)

saveRDS(object = df_week,
        file = '../df_week_ref.rds')

# expand with codes for counties
df_cnt_week <-
  expand.grid(sundays=df_week$sundays,
              GBcode_cnt=as.character(counties$GBcode_cnt),
              stringsAsFactors = F)
str(df_cnt_week)

# save 
saveRDS(object = df_cnt_week,
        file = '../df_cnt_week_ref.rds')

#'
#' ## define reference for day and county-day ##
#'

# define initial dataframe for weeks
df_day <- data.frame(
  day=
    format(seq.Date(from=as.Date('2003-01-01'),
                    to=as.Date('2014-12-31'),
                    by="day")),
  stringsAsFactors = F
)

str(df_day)

saveRDS(object = df_day,
        file = '../df_day_ref.rds')

# expand with codes for counties
df_cnt_day <-
  expand.grid(day=df_day$day,
              GBcode_cnt=as.character(counties$GBcode_cnt),
              stringsAsFactors = F)
str(df_cnt_day)

# save 
saveRDS(object = df_cnt_day,
        file = '../df_cnt_day_ref.rds')


