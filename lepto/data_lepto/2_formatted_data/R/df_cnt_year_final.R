#' ---
#' title: "Create dataframe with all variables at county-yearly resolution"
#' author: "Karina Cucchi"
#' date: "September 21st, 2017"
#' ---
#' 
#' 
#' 
#' I want to create a dataframe where each line correponds to:
#' * a given county
#' * a given year
#' * population in county
#' * whether the county contains at least one case
#' * number of lepto cases
#' * mean rainfall in year
#' * mean temperature in year
#' * mean runoff in year
#' * mean soil moisture in year
#' 

library(lubridate)
library(dplyr)
library(reshape2)


#' # Load yearly/county data #

#' ## load reference county-yearly dataframe ##

df_cnt_year <- readRDS('../df_cnt_year_ref.rds')

#' ## load yearly/county population data ##

df_pop <- readRDS('../df_cnt_year_pop.rds')
str(df_pop)

#' ## load yearly/county health data ##

df_lep <- readRDS('../df_cnt_year_lep.rds')
str(df_lep)
# Each row is a county/year
# CNTY_CODE is the corresponding county idx
# year is the year
# count is human lepto count
# count_farmers is human lepto count restricted to farmers
# NAs before 2003 because surveillance started in 2003

#'
#' ## load cnt-level statistics ##
#'

df_cnt_lep <- readRDS('../df_cnt_lep.rds')
str(df_cnt_lep)

#' 
#' ## load yearly/county rainfall data ##
#' 

df_cmorph <- readRDS('../df_cnt_year_cmorph.rds')
str(df_cmorph)
# Each row is a county/year
# CNTY_CODE is the corresponding county idx
# year is the year
# cmorph_mmPday is the average daily rainfall

#'
#' ## load yearly/county temperature data ##
#'

df_temp <- readRDS('../df_cnt_year_berkeleyearth.rds')
str(df_temp)
# Each row is a county/year
# CNTY_CODE is the corresponding county idx
# year is the year
# temp_C_m1m2 is the average temperature between m1 and m2 in that year

#'
#' ## load yearly/county vic data ##
#'

df_vic <- readRDS('../df_cnt_year_vic.rds')
str(df_vic)

#'
#'  # Merge data #
#'  

# add population
df_cnt_year <- 
  dplyr::left_join(x = df_cnt_year,
                   y = df_pop,
                   by=c("year","CNTY_CODE"))


# add lep counts
df_cnt_year <- 
  dplyr::left_join(x = df_cnt_year,
                   y = df_lep,
                   by=c("year","CNTY_CODE"))

# add county-level lep statistics
df_cnt_year <- 
  dplyr::left_join(x = df_cnt_year,
                   y = df_cnt_lep[,
                                  setdiff(names(df_cnt_lep),
                                          c("count","count_farmers"))],
                   by = c("CNTY_CODE"))

# add rainfall from cmorph
df_cnt_year <- 
  dplyr::left_join(x = df_cnt_year,
                   y = df_cmorph,
                   by=c("year","CNTY_CODE"))

# add temperature from berkeley earth project
df_cnt_year <- 
  dplyr::left_join(x = df_cnt_year,
                   y = df_temp,
                   by=c("year","CNTY_CODE"))

# add vic variables
df_cnt_year <-
  dplyr::left_join(x = df_cnt_year,
                   y = df_vic,
                   by=c("year","CNTY_CODE"))

str(df_cnt_year)


#'
#' # Save final variable #
#'

saveRDS(object = df_cnt_year,file = '../df_cnt_year_final.rds')



