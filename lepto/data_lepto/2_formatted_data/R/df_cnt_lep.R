#' ---
#' title: "Statistics of lepto data at the county level"
#' author: "Karina Cucchi"
#' date: "September 13th, 2017"
#' ---
#' 
#' In this script I extract stats for lepto data 
#' at county-level.
#' 

source('../../refs.R')

library(sp)
library(dplyr)
library(GISTools)

#'
#' # Load data #
#' 

# load lepto spatial dataset
sp_lep <- readRDS(file = '../../1_clean_data/sichuan_database/sp_lep.rds')
str(sp_lep@data)

# load lepto descriptive dataset (for occupation information)
df_lep_gdb <- readRDS(file = '../../1_clean_data/sichuan_database/df_lep_gdb.rds')

# load county dataset
counties <- readRDS(file = '../../1_clean_data/sichuan_adm/counties_ref.rds')
counties$CNTY_CODE <- as.character(counties$CNTY_CODE)

# projection sanity check
proj4string(counties) == proj4string(sp_lep)

#'
#' # Join occupation to sp ##
#'

df_lep_gdb$isFarmer <- (df_lep_gdb$occupation_trns == 'Farmer')
sp_lep$isFarmer <- 
  dplyr::left_join(x = sp_lep@data,
                   y = df_lep_gdb[,c('OBJECTID','isFarmer')],
                   by="OBJECTID")$isFarmer
#'
#' # Counts #
#'

# count number of points by county (total counts)
counties$count <-
  GISTools::poly.counts(pts = sp_lep,polys = counties)

# count number of points by county (farmers only)
counties$count_farmers <-
  GISTools::poly.counts(pts = sp_lep[which(sp_lep@data$isFarmer),],polys = counties)

df_cnt_lep <- counties@data

head(df_cnt_lep)

str(df_cnt_lep)

#'
#' # Define statistics #
#'

#'
#' ## Add whether the county has at least one count ##
#'

df_cnt_lep$cntWithLep <- (df_cnt_lep$count > 0)

df_cnt_lep$cntWithLep_farmers <- (df_cnt_lep$count_farmers > 0)

head(df_cnt_lep)

#'
#' ## Add field for rank by lepto ##
#'

# create field indicating 1st, 2nd etc county with most lep
df_cnt_lep$rank_byLep <-
  round(rank(-df_cnt_lep$count))

# create field indicating 1st, 2nd etc county with most lep
df_cnt_lep$rank_byLep_farmers <-
  round(rank(-df_cnt_lep$count_farmers))

# check 
df_cnt_lep[which(df_cnt_lep$rank_byLep %in% 1:5),]
df_cnt_lep[which(df_cnt_lep$rank_byLep_farmers %in% 1:5),]

str(df_cnt_lep)

#'
#' # Save final variable #
#'

saveRDS(object = df_cnt_lep,file = '../df_cnt_lep.rds')