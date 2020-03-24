# reads lepto data from csv
# saves it a spatialpointdataframe in rds file

library(sp) # for manipulating spatial objects
library(lubridate) # for manipulating dates

# read reference file
source('../../../refs.R')

#' 
#' # Read data from csv file #
#' 

pathToRaw <- '../../../0_raw_data/sichuan_database/'

lep <- read.csv(paste0(pathToRaw,'cases_XY_UTM.csv'),
                stringsAsFactors = F)
names(lep)

#'
#' # Convert dataframe to SpatialPointsDataFrame #
#' 

coordinates(lep) <- c("POINT_X", "POINT_Y") 

# add projection to lep (guessed from the name of the file)
prj <- CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(lep) <- prj

# convert to reference CRS
lep_test <- spTransform(x = lep,CRSobj = ref_CRS)

plot(lep,pch=19)

#' 
#' # Get rid of 0,0 coordinates #
#' 


idx_zeros <- unique(which(coordinates(lep)==0, arr.ind = T)[,'row'])
lep <- lep[-idx_zeros,]
plot(lep,pch=19)

#'
#' # add week of diagnosis in dataframe for later analysis #
#'

vect_dates <- as.Date(x = lep$date_diagn,format='%m/%d/%Y')
lep$week_diag <- format(x = vect_dates,format="%U")

#'
#' # add date of preceding sunday for later analyses #
#'

lep$sundays <- format(x = lubridate::floor_date(x = vect_dates,unit = "week"),
                      format = "%Y-%m-%d")

#'
#' # get rid of unnecessary fields #
#'

head(lep)
lep <- lep[,c("OBJECTID","date_diagn","birthday",
              "year_diag","month_diag","week_diag","sundays")]
head(lep)

#'
#' # Save lepto data as rds file #
#'  


saveRDS(object = lep,file = "sp_lep.rds")


#'
#' # check : plot with reference county map #
#'

counties <- readRDS('../../sichuan_adm/counties_ref.rds')
proj4string(counties)
proj4string(lep)

plot(lep)
plot(counties,add=T)
# it's ok