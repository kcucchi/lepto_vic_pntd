#'
#'---
#'title: Extract time series of vic variables
#'author: Karina Cucchi
#'date: March 27th, 2018
#'---
#'
#'
#'In this script I derive yearly and weekly 
#'time series of VIC data. 
#'I select only water basins containing lepto cases.
#'

library(dplyr)
library(sp)
library(raster)
library(lubridate)

#'
#' # Load data #
#' 

#'
#' load basins with lepto cases
#'

sp_area <- 
  readRDS(
    file = '../sp_north.rds')
proj4string(sp_area)

#'
#' load and process vic data
#' 

names_vic <- 
  c('sm1','sm2','sm3','roff',
    'Prcp','Tmin','Tmax')

#' at yearly resolution

# load reference year dataframe
# df_year <- 
#   readRDS(file = '../../2_formatted_data/df_year_ref.rds')
# load years corresponding to historical data
df_year <-
  data.frame(year=as.character(1952:2003))

# loop over vic variables
i_var = 2
for(i_var in 1:length(names_vic)){
  
  cat('\n',i_var)
  
  var <- names_vic[i_var]
  
  r <- 
    readRDS(paste0('../../../../data/1_clean_data/sichuan_hydroDB/raster_vic_zhang_',
                   var,'.rds'))
  proj4string(r)
  
  # define dates corresponding to each layer
  r_dates <- as.Date(names(r),format = "X%Y.%m.%d")
  
  #
  # ### sum by year ###
  #
  
  # The extract function is quite slow.
  # First, sum by year over raster 
  # so that I end up with 13 years going from 2003 to 2015
  
  # prepare for summation by year
  # extract unique index corresponding to year
  r_years <- as.numeric(format(r_dates,format="%Y"))
  # get index corresponding to year
  r_yearIdx <- as.numeric(as.factor(r_years))
  # get years corresponding to indices
  r_yearNames <- levels(as.factor(r_years))
  
  # actually take the mean layers by year
  r_byYear <- raster::stackApply(r, r_yearIdx, fun = mean)
  names(r_byYear) <- r_yearNames
  
  #
  # ### extract ###
  #
  
  # extract yearly value at each county
  vect_year_extracted <- 
    raster::extract(x = r_byYear, y = sp_basins,
                    fun="mean", # mean over county
                    na.rm=TRUE,method="simple")
  
  df_year_extracted <- 
    data.frame(year=substr(x = colnames(vect_year_extracted),
                           start = 2,stop = 5),
               vic_var=as.numeric(vect_year_extracted),
               stringsAsFactors = F)
  
  #
  # # join to results dataframe
  #
  
  df_year <- 
    dplyr::left_join(x = df_year,
                     y = df_year_extracted,
                     by = "year")
  # modify name of variable
  names(df_year)[which(names(df_year)=="vic_var")] <- 
    paste0('vic_',var)
  
}

head(df_year)

#' same at weekly resolution

# load reference year dataframe
df_week <- 
  readRDS(file = '../../2_formatted_data/df_week_ref.rds')

# loop over vic variables
i_var = 5
for(i_var in 1:length(names_vic)){
  
  cat('\n',i_var)
  
  var <- names_vic[i_var]
  
  r <- 
    readRDS(paste0('../../1_clean_data/sichuan_hydroDB/raster_vic_zhang_',var,'.rds'))
  proj4string(r)
  
  # define dates corresponding to each layer
  r_dates <- as.Date(names(r),format = "X%Y.%m.%d")
  
  #
  # ### sum by week ###
  #
  
  # r_dates contains dates for each layer in the raster
  # calculate date for each preceding sunday with package lubridate
  r_sundays <- 
    lubridate::floor_date(x=r_dates,unit = "weeks")
  # get index corresponding to year
  r_sundaysIdx <- 
    as.numeric(as.factor(r_sundays))
  # get weeks corresponding to indices
  r_sundaysNames <- 
    levels(as.factor(r_sundays))
  
  
  # actually take the mean layers by week
  r_byWeek <- 
    raster::stackApply(r, r_sundaysIdx, fun = mean)
  names(r_byWeek) <- r_sundaysNames
  
  #
  # ### extract ###
  #
  
  # extract weekly rainfall
  # this takes ~7s
  vect_week_extracted <- 
    raster::extract(x = r_byWeek, y = sp_basins,
                    fun="mean", # mean over county
                    na.rm=TRUE,method="simple")
  
  df_week_extracted <- 
    data.frame(sundays=format(as.Date(x = colnames(vect_week_extracted),
                                      format = "X%Y.%m.%d")),
               vic_var=as.numeric(vect_week_extracted),
               stringsAsFactors = F)
  
  #
  # # join to results dataframe
  #
  
  df_week <- 
    dplyr::left_join(x = df_week,
                     y = df_week_extracted,
                     by = "sundays")
  # modify name of variable
  names(df_week)[which(names(df_week)=="vic_var")] <- 
    paste0('vic_',var)
  
}

head(df_week)

#'
#' # Save timeseries #
#'

# save yearly timeseries
saveRDS(object = df_year,file = '../df_year_vic.rds')

# save weekly timeseries
saveRDS(object = df_week,file = '../df_week_vic.rds')
