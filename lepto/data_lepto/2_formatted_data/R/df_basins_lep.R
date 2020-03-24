#' ---
#' title: "Statistics of lepto data at the basin level"
#' author: "Karina Cucchi"
#' date: "September 13th, 2017"
#' ---
#' 
#' In this script I extract stats for lepto data 
#' at basin-level.
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

# load basin dataset
basins <- readRDS(file = '../../1_clean_data/sichuan_hydroDB/basins_waterbase.rds')


# projection sanity check
proj4string(basins) == proj4string(sp_lep)

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

# count number of points by basin (total counts)
basins$count <-
  GISTools::poly.counts(pts = sp_lep,polys = basins)

# count number of points by county (farmers only)
basins$count_farmers <-
  GISTools::poly.counts(pts = sp_lep[which(sp_lep@data$isFarmer),],polys = basins)

df_basins_lep <- basins@data

head(df_basins_lep)

str(df_basins_lep)

#'
#' # Define statistics #
#'

#'
#' ## Add whether the county has at least one count ##
#'

df_basins_lep$cntWithLep <- (df_basins_lep$count > 0)

df_basins_lep$cntWithLep_farmers <- (df_basins_lep$count_farmers > 0)

head(df_basins_lep)

#'
#' ## Add field for rank by lepto ##
#'

# create field indicating 1st, 2nd etc county with most lep
df_basins_lep$rank_byLep <-
  round(rank(-df_basins_lep$count))

# create field indicating 1st, 2nd etc county with most lep
df_basins_lep$rank_byLep_farmers <-
  round(rank(-df_basins_lep$count_farmers))

# check 
df_basins_lep[which(df_basins_lep$rank_byLep %in% 1:5),]
df_basins_lep[which(df_basins_lep$rank_byLep_farmers %in% 1:5),]

str(df_basins_lep)

#'
#' # Save final variable #
#'

saveRDS(object = df_basins_lep,file = '../df_basins_lep.rds')
