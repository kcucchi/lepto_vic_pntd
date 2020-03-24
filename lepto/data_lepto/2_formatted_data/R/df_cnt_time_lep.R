#' ---
#' title: "Extracts lepto data at the county level"
#' author: "Karina Cucchi"
#' date: "September 7th, 2016"
#' ---
#' 
#' In this script I load and format lepto data 
#' for further exploration at county-level.
#' 

source('../../refs.R')

library(sp)
library(dplyr)
library(lubridate) # for date manipulation

#'
#' # Load data #
#' 

# load lepto spatial dataset
sp_lep <- readRDS(file = '../../1_clean_data/sichuan_database/sp_lep.rds')
str(sp_lep@data)

# load lepto descriptive dataset
df_lep_gdb <- 
  readRDS(file = '../../1_clean_data/sichuan_database/df_lep_gdb.rds')

# load county dataset
counties <- 
  readRDS(file = '../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')
str(counties@data)

# projection sanity check
proj4string(counties) == proj4string(sp_lep)

# load reference county-time dataframes
df_cnt_year <- readRDS(file = '../df_cnt_year_ref.rds')
df_cnt_week <- readRDS(file = '../df_cnt_week_ref.rds')
df_cnt_day <- readRDS(file = '../df_cnt_day_ref.rds')

#'
#' # Join fields to individual cases in sp_lep #
#' 

#'
#' ## Join county codes ##
#'

# get county code using over function from sp package
sp_lep$GBcode_cnt <- 
  as.character(sp::over(x = sp_lep,y = counties)$GBcode_cnt)
head(sp_lep)

#'
#' ## Join occupation data ##
#'

df_lep_gdb$isFarmer <- (df_lep_gdb$occupation_trns == 'Farmer')
head(df_lep_gdb)

sp_lep$isFarmer <- 
  dplyr::left_join(x = sp_lep@data,
                   y = df_lep_gdb[,c('OBJECTID','isFarmer')],
                   by="OBJECTID")$isFarmer


#'
#' # count per county-year, county-week and county-day #
#'

#'
#' ## first do county-year ##
#'

# count total cases by county-year
lep_byCntYear <- 
  dplyr::summarise(
    dplyr::group_by(sp_lep@data,year,GBcode_cnt),
    count=n())
str(lep_byCntYear)

# count farmer cases by county-year
lep_byCntYear_farmers <- 
  dplyr::summarise(
    dplyr::group_by(subset(x = sp_lep@data,subset = isFarmer),
                    year,GBcode_cnt),
    count_farmers=n())
lep_byCntYear <- dplyr::left_join(x = lep_byCntYear,
                                  y = lep_byCntYear_farmers,
                                  by=c("year","GBcode_cnt"))
rm(lep_byCntYear_farmers)
str(lep_byCntYear)


# join to reference cnt_year dataframe
df_cnt_year_lep <- 
  dplyr::left_join(x=df_cnt_year,
                   y=lep_byCntYear,
                   by=c("year", "GBcode_cnt"))
# replace NAs during 2004-2014 period by zeros
# first everyone
idx <- which(is.na(df_cnt_year_lep$count) & 
               as.numeric(df_cnt_year_lep$year) >= 2004)
df_cnt_year_lep$count[idx] <- 0
# now just farmers
idx <- which(is.na(df_cnt_year_lep$count_farmers) 
             & as.numeric(df_cnt_year_lep$year) >= 2004)
df_cnt_year_lep$count_farmers[idx] <- 0
str(df_cnt_year_lep)

# check that same number of points in original dataset and in summary by county-year
nrow(sp_lep) - sum(df_cnt_year_lep$count,na.rm = T)
# 8 data points are outside of the sichuan region

plot(sp_lep)
plot(counties,add=T)

# save 
saveRDS(object = df_cnt_year_lep,
        file = '../df_cnt_year_lep.rds')

#'
#' ## now do county-week ##
#'

# count all cases by county-week
lep_byCntWeek <- dplyr::summarise(
  dplyr::group_by(sp_lep@data,sundays,GBcode_cnt),
  count=n())
str(lep_byCntWeek)

# count farmer cases by county-week
lep_byCntWeek_farmers <- 
  dplyr::summarise(
    dplyr::group_by(subset(x = sp_lep@data,subset = isFarmer),
                    sundays,GBcode_cnt),
    count_farmers=n())
lep_byCntWeek <- dplyr::left_join(x = lep_byCntWeek,
                                  y = lep_byCntWeek_farmers,
                                  by=c("sundays","GBcode_cnt"))
rm(lep_byCntWeek_farmers)
str(lep_byCntWeek)

# join to reference cnt_week dataframe
df_cnt_week_lep <- 
  dplyr::left_join(x=df_cnt_week,
                   y=lep_byCntWeek,
                   by=c("sundays", "GBcode_cnt"))

# replace NAs during 2004-2014 period by zeros
# first everyone
idx <- 
  which(is.na(df_cnt_week_lep$count) & 
          lubridate::year(as.Date(df_cnt_week_lep$sundays)) >= 2004)
df_cnt_week_lep$count[idx] <- 0
# then just farmers
idx <- 
  which(is.na(df_cnt_week_lep$count_farmers) & 
          lubridate::year(as.Date(df_cnt_week_lep$sundays)) >= 2004)
df_cnt_week_lep$count_farmers[idx] <- 0
str(df_cnt_week_lep)

# check that same number of points in original dataset and in summary by county-year
nrow(sp_lep) - sum(df_cnt_week_lep$count,na.rm = T)
# 8 data points are outside of the sichuan region

# save 
saveRDS(object = df_cnt_week_lep,
        file = '../df_cnt_week_lep.rds')

#'
#' ## finally do county-day ##
#'

head(sp_lep@data)

sp_lep$day <- format(as.Date(sp_lep$date_diagn,format="%m/%d/%Y 0:00"))

head(sp_lep@data)

# count all cases by county-week
lep_byCntDay <- dplyr::summarise(
  dplyr::group_by(sp_lep@data,day,GBcode_cnt),
  count=n())
str(lep_byCntDay)

# count farmer cases by county-week
lep_byCntDay_farmers <- 
  dplyr::summarise(
    dplyr::group_by(subset(x = sp_lep@data,subset = isFarmer),
                    day,GBcode_cnt),
    count_farmers=n())
lep_byCntDay <- dplyr::left_join(x = lep_byCntDay,
                                  y = lep_byCntDay_farmers,
                                  by=c("day","GBcode_cnt"))
rm(lep_byCntDay_farmers)
str(lep_byCntDay)

# join to reference cnt_day dataframe
df_cnt_day_lep <- 
  dplyr::left_join(x=df_cnt_day,
                   y=lep_byCntDay,
                   by=c("day", "GBcode_cnt"))

# replace NAs during 2004-2014 period by zeros
# first everyone
idx <- 
  which(is.na(df_cnt_day_lep$count) & 
          lubridate::year(as.Date(df_cnt_day_lep$day)) >= 2004)
df_cnt_day_lep$count[idx] <- 0
# then just farmers
idx <- 
  which(is.na(df_cnt_day_lep$count_farmers) & 
          lubridate::year(as.Date(df_cnt_day_lep$day)) >= 2004)
df_cnt_day_lep$count_farmers[idx] <- 0
str(df_cnt_day_lep)

# check that same number of points in original dataset and in summary by county-year
nrow(sp_lep) - sum(df_cnt_day_lep$count,na.rm = T)
# 8 data points are outside of the sichuan region

# save 
saveRDS(object = df_cnt_day_lep,
        file = '../df_cnt_day_lep.rds')
