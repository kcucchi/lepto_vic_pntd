#' ---
#' title: "Extracts lepto data at the township level"
#' author: "Karina Cucchi"
#' date: "December 21st, 2017"
#' ---
#' 
#' In this script I load and format lepto data 
#' for further exploration at township-level.
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
df_lep_gdb <- readRDS(file = '../../1_clean_data/sichuan_database/df_lep_gdb.rds')

# load township dataset
townships <- readRDS(file = '../../1_clean_data/sichuan_adm/townships_ref.rds')

# projection sanity check
proj4string(townships) == proj4string(sp_lep)

#'
#' # Count per day #
#'

#'
#' ## Define reference township-day dataframes ##
#'

# example
str(readRDS(file = '../df_cnt_week_ref.rds'))

vect_days <- 
  seq.Date(from = as.Date('2004/01/01'),
           to = as.Date('2014/12/31'),
           by = 'day')

df_twn_day <-
  expand.grid(day=format(vect_days),
              TWN_CODE=townships@data$TWN_CODE,
              stringsAsFactors = F)

str(df_twn_day)

#'
#' ## Join fields to individual cases in sp_lep ##
#' 

head(sp_lep)

#'
#' ## add formatted dates
#'

sp_lep$day <-
  format(x = as.Date(sp_lep$date_diagn,format = '%m/%d/%Y'))


#'
#' ## Join township codes 
#'

# get township code using over function from sp package
sp_lep$TWN_CODE <- 
  as.character(sp::over(x = sp_lep,y = townships)$TWN_CODE)
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
#' ## final count per township-day ##
#'

# count total cases by township-day
lep_byTwnDay <- 
  dplyr::summarise(
    dplyr::group_by(sp_lep@data,day,TWN_CODE),
    count=n())
str(lep_byTwnDay)

# count farmer cases by township-day
lep_byTwnDay_farmers <- 
  dplyr::summarise(
    dplyr::group_by(subset(x = sp_lep@data,subset = isFarmer),
                    day,TWN_CODE),
    count_farmers=n())
lep_byTwnDay <- dplyr::left_join(x = lep_byTwnDay,
                                 y = lep_byTwnDay_farmers,
                                 by=c("day","TWN_CODE"))
rm(lep_byTwnDay_farmers)
str(lep_byTwnDay)


# join to reference twn_day dataframe
df_twn_day_lep <- 
  dplyr::left_join(x=df_twn_day,
                   y=lep_byTwnDay,
                   by=c("day","TWN_CODE"))

# replace NAs during 2004-2014 period by zeros
# first everyone
idx <- which(is.na(df_twn_day_lep$count))
df_twn_day_lep$count[idx] <- 0
# now just farmers
idx <- which(is.na(df_twn_day_lep$count_farmers))
df_twn_day_lep$count_farmers[idx] <- 0
str(df_twn_day_lep)

# check that same number of points in original dataset and in summary by county-year
nrow(sp_lep) - sum(df_twn_day_lep$count,na.rm = T)
# 8 data points are outside of the sichuan region

plot(sp_lep,col='red')
plot(townships,add=T)

# save 
saveRDS(object = df_twn_day_lep,
        file = '../df_twn_day_lep.rds')


#'
#' # Count per year #
#'

vect_year <- 2004:2014

df_twn_year <-
  expand.grid(year=format(vect_year),
              TWN_CODE=townships@data$TWN_CODE,
              stringsAsFactors = F)


#'
#' ## count per township-year ##
#'

lep_byTwnYr <- 
  data.frame(sp_lep@data %>% 
               group_by(year,TWN_CODE) %>% 
               summarize(count=n()))

lep_byTwnYr_farmers <- 
  data.frame(subset(sp_lep@data,isFarmer) %>% 
               group_by(year,TWN_CODE) %>% 
               summarize(count_farmers=n()))
lep_byTwnYr <- 
  dplyr::left_join(x = lep_byTwnYr,
                   y=lep_byTwnYr_farmers,
                   by=c("year","TWN_CODE"))

rm(lep_byTwnYr_farmers)
str(lep_byTwnYr)

# join to reference twn_year dataframe
df_twn_year_lep <-
  dplyr::left_join(x = df_twn_year,
                   y = lep_byTwnYr,
                   by=c("year","TWN_CODE"))

# replace NAs by zeros
idx <- which(is.na(df_twn_year_lep$count))
df_twn_year_lep$count[idx] <- 0
# now just farmers
idx <- which(is.na(df_twn_year_lep$count_farmers))
df_twn_year_lep$count_farmers[idx] <- 0
str(df_twn_year_lep)

# save
saveRDS(object = df_twn_year_lep,
        file = '../df_twn_year_lep.rds')


