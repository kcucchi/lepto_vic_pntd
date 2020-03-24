# reads lepto data as exported from gdb by myself
# saves it a dataframe in rds file

# read reference file
source('../../../refs.R')

#' 
#' ## read data from csv file ##
#' 

# weird read.csv skips rows when I read directly from the copied-paste csv from attribute table
# fixed when I open and save from Excel
lep_gdb <- read.csv('../../../0_raw_data/sichuan_database/all_cases_trns_test.csv',
                   stringsAsFactors = F)

# check that 2939 cases such as in the database
nrow(lep_gdb)

head(lep_gdb)

#' 
#' ## fix problem in date_diagn ##
#' 

# fields date_diagn and time_diagn are mutually exclusive... 
# its actually coming from the attribute table of shapefile all_cases
# -> not my fault

# replace empty field of date_diagn by fields in time_diagn
idx_na_date_diagn <- which(lep_gdb$date_diagn=="")
lep_gdb$date_diagn[idx_na_date_diagn] <- lep_gdb$time_diagn[idx_na_date_diagn]
rm(idx_na_date_diagn)
# check that no empty field now in date_diagn
length(which(lep_gdb$date_diagn==""))

#' 
#' ## complete years field ##
#' 

# missing years in years field.
# add years from date_diagn field

# first get indices with missing diagnosed years
idx = which(lep_gdb$year=="0")

# looks like all indices correspond to year 2004
# transform to date type
dates_idx <- as.Date(lep_gdb$date_diagn[idx],format = "%m/%d/%Y %H:%M")
# get years from the dates
lep_gdb$year[idx] <- format(dates_idx,format="%Y")
rm(dates_idx,idx)

# now no zero field !
unique(lep_gdb$year)

#' 
#' ## calculate field for diagnosed week in year + add sundays field ##
#' 

#' sundays contains the date of the sunday preceding the infection

temp <- as.Date(lep_gdb$date_diagn,format = "%m/%d/%Y %H:%M")
lep_gdb$week <- as.numeric(format(x = temp,format="%U"))
lep_gdb$sundays <- as.character(lubridate::floor_date(temp,unit="week"))
rm(temp)

#' 
#' ## complete age field ##
#' 

# fields without years also don't have age
idx = which(lep_gdb$age == " ")

# get year from birthday
dates_birth <- as.Date(lep_gdb$birthday[idx],format = "%m/%d/%Y %H:%M")
year_birth <- as.numeric(format(dates_birth,format="%Y"))

# fill in corresponding value of age at illness
lep_gdb$age[idx] <- as.numeric(lep_gdb$year[idx]) - year_birth

rm(year_birth,dates_birth,idx)

unique(lep_gdb$age)
# remove " years old" from the fields
lep_gdb$age <- gsub(pattern = " years old",replacement = "",x = lep_gdb$age)
unique(lep_gdb$age)

#'
#' ## get rid of unnecessary fields ##
#'

head(lep_gdb)

# fields to keep in the final export
keep_fields <- c("OBJECTID","Gender","occupation_trns","age",
                 "date_diagn", "date_death",
                 "year","week","sundays")
# fields corresponding to the spatial location of points
xy_fields <- c("x1","y1","x2","y2","x_utm","y_utm")

lep_gdb <- lep_gdb[,c(keep_fields,xy_fields)]

#' 
#' ## now transform to 2 spatialpointsdataframe for 2 sets of coordinates ##
#' 

library(sp)

# transform to object of sp class
# first transform coordinate fields to numeric vector 
for(col in c('x1','y1','x2','y2','x_utm','y_utm')){
  lep_gdb[,col] <- as.numeric(lep_gdb[,col])
}
rm(col)
# then get rid of zeros (no zeros for utm)
idx1 <- intersect(which(lep_gdb$x1 != 0),which(lep_gdb$y1!=0))
idx2 <- intersect(which(lep_gdb$x2 != 0),which(lep_gdb$y2!=0))
sp_lep_gdb_1 <- lep_gdb[idx1,];sp_lep_gdb_2 <- lep_gdb[idx2,]
rm(idx1,idx2)
# now transform to sp class
coordinates(sp_lep_gdb_1) <- c('x1','y1')
proj4string(sp_lep_gdb_1) <- CRS("+proj=longlat")
coordinates(sp_lep_gdb_2) <- c('x2','y2')
proj4string(sp_lep_gdb_2) <- CRS("+proj=longlat")
sp_lep_gdb <- lep_gdb
coordinates(sp_lep_gdb) <- c('x_utm','y_utm')
proj4string(sp_lep_gdb) <- 
  CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


# change projection to reference projection
sp_lep_gdb_1 <- spTransform(x = sp_lep_gdb_1, CRSobj = ref_CRS)
sp_lep_gdb_2 <- spTransform(x = sp_lep_gdb_2, CRSobj = ref_CRS)
sp_lep_gdb <- spTransform(x = sp_lep_gdb, CRSobj = ref_CRS)

# check size of corresponding subsets
nrow(sp_lep_gdb)
nrow(sp_lep_gdb_1)
nrow(sp_lep_gdb_2)


# check by plotting

# load sichuan file

sichuan <-
  readRDS('../../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')

plot(sichuan)
plot(sp_lep_gdb_1,add=T)
plot(sp_lep_gdb_2,add=T,col='red')
plot(sp_lep_gdb,add=T,col='green',pch=2)

plot(sp_lep_gdb,col='green',pch=2)
plot(sichuan,add=T)

# I don't understand the x_utm y_utm fields, just use lep_gdb_1.rds
rm(sp_lep_gdb)


#' 
#' # save objects as rds files #
#' 

# do not save because reference lepto dataset will contain only one spatial projection
# saveRDS(object = lep_gdb,file = '../RData/lep_gdb_df.rds')
saveRDS(object = sp_lep_gdb_1[,keep_fields],
        file = 'sp_lep_gdb_1.rds')
saveRDS(object = sp_lep_gdb_2[,keep_fields],
        file = 'sp_lep_gdb_2.rds')


