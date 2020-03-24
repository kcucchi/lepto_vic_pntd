#' ---
#' title: "Saves final lepto dataset"
#' subtitle: "(1) sp objects with locations (2) R dataframe with gdb covariates"
#' author: "Karina Cucchi"
#' date: "September 7th, 2017"
#' ---
#' 

library(sp)

#'
#' # Read data #
#'

# export everything from gdb1

lep <- readRDS('sp_lep_gdb_1.rds')

#'
#' # Save data #
#' 

head(lep)

dim(lep)

# save spatial coordinates
saveRDS(object = lep[,c("OBJECTID","date_diagn","year","week","sundays")],
        file = '../../sichuan_database/sp_lep.rds')

# save covariates from gdb
saveRDS(object = lep@data,file = "../../sichuan_database/df_lep_gdb.rds")
