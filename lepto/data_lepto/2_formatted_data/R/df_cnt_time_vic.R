#' ---
#' title: "Extracts vic data at the county level"
#' author: "Karina Cucchi"
#' date: "July 24th, 2016"
#' ---
#' 
#' In this script I load and format vic data 
#' for further exploration at county-level.
#' I extract vic data at yearly and weekly resolution.
#' 

## load packages
library(raster) # for rasters
library(sp) # for spatial objects
library(reshape2) # for dataframe manipulation

# read reference file
source('../../refs.R')

# plotPdf=F

#'
#' # First load relevant data #
#'

#'
#' ## Load counties data ##
#'

counties <- 
  readRDS('../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')
proj4string(counties)

#'
#' ## Load reference cnt_time dataframes ##
#'

# these will also contain the results

df_cnt_year <- readRDS(file = '../df_cnt_year_ref.rds')
df_cnt_week <- readRDS(file = '../df_cnt_week_ref.rds')
df_cnt_day <- readRDS(file = '../df_cnt_day_ref.rds')



#' 
#' ## Load vic data in loop ##
#' 

vect_var <- 
  c('sm1','sm2','sm3','roff',
    'Prcp','Tmin','Tmax')

i_var=2

for(i_var in 1:length(vect_var)){
  
  cat('\n',i_var)
  
  var <- vect_var[i_var]
  
  r <- 
    readRDS(paste0('../../../../data/1_clean_data/sichuan_hydroDB/raster_vic_zhang_',
                   var,'.rds'))
  proj4string(r)
  
  # define dates corresponding to each layer
  r_dates <- as.Date(names(r),format = "X%Y.%m.%d")
  
  ########################
  # at yearly resolution #
  ########################
  
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
  df_cnt_year_extracted <- 
    raster::extract(r_byYear, counties,
                    fun="mean", # mean over county
                    na.rm=TRUE,method="simple")
  # df_cnt_year_extracted is a matrix containing mean daily value for each year
  # each row is a county
  rownames(df_cnt_year_extracted) <- counties$GBcode_cnt
  # each column is a year
  colnames(df_cnt_year_extracted) <- r_yearNames
  
  #
  # ### reshape ###
  #
  
  df_cnt_year_extracted_long <- 
    reshape2::melt(data = df_cnt_year_extracted)
  
  # check output
  str(df_cnt_year_extracted_long)
  
  # modify names of fields
  names(df_cnt_year_extracted_long) <- 
    c("GBcode_cnt","year","var")
  str(df_cnt_year_extracted_long)
  
  # modify to character
  df_cnt_year_extracted_long$GBcode_cnt <- 
    as.character(df_cnt_year_extracted_long$GBcode_cnt)
  df_cnt_year_extracted_long$year <- 
    as.character(df_cnt_year_extracted_long$year)
  str(df_cnt_year_extracted_long)
  
  #
  # ### join to reference dataframe ##
  #
  
  df_cnt_year <-
    dplyr::left_join(x = df_cnt_year,
                     y = df_cnt_year_extracted_long,
                     by = c("year","GBcode_cnt"))
  
  # change name of variable
  names(df_cnt_year)[which(names(df_cnt_year)=="var")] <- 
    paste0('vic_',var)
  
  str(df_cnt_year)
  
  
  
  ########################
  # at weekly resolution #
  ########################
  
  #'
  #' ### sum by week ###
  #'
  
  # r_dates contains dates for each layer in the raster
  # calculate date for each preceding sunday with package lubridate
  r_sundays <- 
    lubridate::floor_date(x=r_dates,unit = "week")
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
  
  #'
  #' ### extract ###
  #'
  
  # extract weekly rainfall at each county
  # this takes ~7s
  df_cnt_week_extracted <- 
    raster::extract(r_byWeek, counties,
                    fun="mean", # mean over county
                    na.rm=TRUE,method="simple")
  # df_cnt_week_extracted is a matrix containing
  # mean weekly rainfall for each week
  # each row is a county
  rownames(df_cnt_week_extracted) <- counties$GBcode_cnt
  #each column is a week
  colnames(df_cnt_week_extracted) <- r_sundaysNames
  
  #'
  #' ### reshape ###
  #'
  
  df_cnt_week_extracted_long <- 
    reshape2::melt(data = df_cnt_week_extracted)
  
  # check output
  str(df_cnt_week_extracted_long)
  
  # modify names of df_cnt_week_extracted_long
  names(df_cnt_week_extracted_long) <- 
    c("GBcode_cnt","sundays","var")
  str(df_cnt_week_extracted_long)
  
  # modify from factor to character
  df_cnt_week_extracted_long$GBcode_cnt <- 
    as.character(df_cnt_week_extracted_long$GBcode_cnt)
  df_cnt_week_extracted_long$sundays <- 
    as.character.factor(df_cnt_week_extracted_long$sundays)
  str(df_cnt_week_extracted_long)
  
  #'
  #' ### join to reference dataframe ##
  #'
  
  df_cnt_week <-
    dplyr::left_join(x = df_cnt_week,
                     y = df_cnt_week_extracted_long,
                     by = c("sundays","GBcode_cnt"))
  
  # change name of variable
  names(df_cnt_week)[which(names(df_cnt_week)=="var")] <- 
    paste0('vic_',var)
  
  str(df_cnt_week)
  
  ########################
  # at daily resolution #
  ########################
 
  
  #'
  #' ### extract ###
  #'
  
  # extract daily rainfall at each county
  # this takes ~7s
  df_cnt_day_extracted <- 
    raster::extract(r, counties,
                    fun="mean", # mean over county
                    na.rm=TRUE,method="simple")
  # df_cnt_day_extracted is a matrix containing
  # mean dayly rainfall for each day
  # each row is a county
  rownames(df_cnt_day_extracted) <- counties$GBcode_cnt
  #each column is a day
  # colnames(df_cnt_day_extracted) <- r_sundaysNames
  
  #'
  #' ### reshape ###
  #'
  
  df_cnt_day_extracted_long <- 
    reshape2::melt(data = df_cnt_day_extracted)
  
  # check output
  str(df_cnt_day_extracted_long)
  
  # modify names of df_cnt_week_extracted_long
  names(df_cnt_day_extracted_long) <- 
    c("GBcode_cnt","day","var")
  str(df_cnt_day_extracted_long)
  
  # modify GBcode_cnt from integer to character
  df_cnt_day_extracted_long$GBcode_cnt <-
    as.character(df_cnt_day_extracted_long$GBcode_cnt)
  
  # modify days from factor to character
  df_cnt_day_extracted_long$day <- 
    as.character.factor(df_cnt_day_extracted_long$day)
  df_cnt_day_extracted_long$day <-
    format(as.Date(x = df_cnt_day_extracted_long$day, format="X%Y.%m.%d"))
  str(df_cnt_day_extracted_long)
  
  #'
  #' ### join to reference dataframe ##
  #'
  
  df_cnt_day <-
    dplyr::left_join(x = df_cnt_day,
                     y = df_cnt_day_extracted_long,
                     by = c("day","GBcode_cnt"))
  
  # change name of variable
  names(df_cnt_day)[which(names(df_cnt_day)=="var")] <- 
    paste0('vic_',var)
  
  str(df_cnt_day)
  
}

#'
#' # save in rds files #
#'

# save yearly dataframe
saveRDS(object = df_cnt_year,
        file = paste0('../df_cnt_year_vic.rds'))

# save weekly dataframe
saveRDS(object = df_cnt_week,
        file = paste0('../df_cnt_week_vic.rds'))


# save weekly dataframe
saveRDS(object = df_cnt_day,
        file = paste0('../df_cnt_day_vic.rds'))


