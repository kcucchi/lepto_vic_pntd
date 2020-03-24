#' ---
#' title: "Statistics of lepto data at the weekly/yearly level"
#' author: "Karina Cucchi"
#' date: "September 18th, 2016"
#' ---
#' 


source('../../refs.R')

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

# load reference time dataframes
df_year <- readRDS(file = '../df_year_ref.rds')
df_week <- readRDS(file = '../df_week_ref.rds')

#'
#' # Join fields to individual cases in sp_lep #
#' 

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
#' # count per year and week #
#'

#'
#' ## first do year ##
#'

# count total cases by year
lep_byYear <- 
  dplyr::summarise(
    dplyr::group_by(sp_lep@data,year),
    count=n())
str(lep_byYear)

# count farmer cases by year
lep_byYear_farmers <- 
  dplyr::summarise(
    dplyr::group_by(subset(x = sp_lep@data,subset = isFarmer),
                    year),
    count_farmers=n())
lep_byYear <- dplyr::left_join(x = lep_byYear,
                               y = lep_byYear_farmers,
                               by=c("year"))
rm(lep_byYear_farmers)
str(lep_byYear)


# join to reference cnt_year dataframe
df_year_lep <- 
  dplyr::left_join(x=df_year,
                   y=lep_byYear,
                   by="year")
# replace NAs during 2004-2014 period by zeros
# first everyone
idx <- which(is.na(df_year_lep$count) & 
               as.numeric(df_year_lep$year) >= 2004)
df_year_lep$count[idx] <- 0
# now just farmers
idx <- which(is.na(df_year_lep$count_farmers) 
             & as.numeric(df_year_lep$year) >= 2004)
df_year_lep$count_farmers[idx] <- 0
str(df_year_lep)

# save 
saveRDS(object = df_year_lep,
        file = '../df_year_lep.rds')

#'
#' ## now do week ##
#'

# count all cases by week
lep_byWeek <- dplyr::summarise(
  dplyr::group_by(sp_lep@data,sundays),
  count=n())
str(lep_byWeek)

# count farmer cases by week
lep_byWeek_farmers <- 
  dplyr::summarise(
    dplyr::group_by(subset(x = sp_lep@data,subset = isFarmer),
                    sundays),
    count_farmers=n())
lep_byWeek <- dplyr::left_join(x = lep_byWeek,
                                  y = lep_byWeek_farmers,
                                  by="sundays")
rm(lep_byWeek_farmers)
str(lep_byWeek)

# join to reference week dataframe
df_week_lep <- 
  dplyr::left_join(x=df_week,
                   y=lep_byWeek,
                   by="sundays")

# replace NAs during 2004-2014 period by zeros
# first everyone
idx <- 
  which(is.na(df_week_lep$count) & 
          lubridate::year(as.Date(df_week_lep$sundays)) >= 2004)
df_week_lep$count[idx] <- 0
# then just farmers
idx <- 
  which(is.na(df_week_lep$count_farmers) & 
          lubridate::year(as.Date(df_week_lep$sundays)) >= 2004)
df_week_lep$count_farmers[idx] <- 0
str(df_week_lep)

# save 
saveRDS(object = df_week_lep,
        file = '../df_week_lep.rds')


