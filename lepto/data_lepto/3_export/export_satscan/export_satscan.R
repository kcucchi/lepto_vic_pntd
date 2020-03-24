

#' ---
#' title: "Exports data for satscan analysis"
#' author: "Karina Cucchi"
#' date: "December 21st, 2017"
#' ---
#' 
#' 

library(sp)
library(ggplot2)
library(dplyr)

export_folder = 'cnt_month'

#'
#' # Population file #
#'

# first do population file: some townships have missing population
# get rid of them for cases and coordinates

# population can be raw population number
townships <- 
  readRDS('../../../../data/1_clean_data/sichuan_adm/townships_ref.rds')

class(townships)
names(townships)

sum(is.na(townships$pop)) # 145 townships with missing population
nrow(townships)

# save codes of towsnhips with missing pop
TWN_CODE_missingPop <- 
  townships@data[which(is.na(townships$pop)),'TWN_CODE']

write.table(x = townships@data[!is.na(townships$pop),],
            file = 'population_file.csv',
            sep = ' ',quote = F,row.names = F)

#'
#' ## population file for counties ##
#'

df_cnt_pop <- 
  readRDS(file = '../../../../data/1_clean_data/sichuan_adm/sichuancountypop/sichuanCountyPop.rds')

# calculate average population by county
df_cnt_pop$pop <-
  (df_cnt_pop$X2014agrpop + df_cnt_pop$X2010pop +
  df_cnt_pop$X2008pop + df_cnt_pop$X2005pop) / 4

write.table(x = df_cnt_pop[,c("GBcode_cnt","pop")],
            file = 'population_file_cnt.csv',
            sep = ' ',quote = F,row.names = F)

#'
#' # Case file #
#'

#'
#' ## export by day ##
#'

#' This file contains a location ID, a number of cases and a date

df_twn_day_lep <-
  readRDS('../../2_formatted_data/df_twn_day_lep.rds')

# change orders of columns for satscan
df_twn_day_lep <- df_twn_day_lep[,c('TWN_CODE','count','day')]

str(df_twn_day_lep)

# check how many cases fall in townships with missing population
df_missing <-
  df_twn_day_lep[which(df_twn_day_lep$TWN_CODE %in% TWN_CODE_missingPop),]
sum(df_missing$count) # that's ok

write.table(x = df_twn_day_lep[!(df_twn_day_lep$TWN_CODE %in% TWN_CODE_missingPop),],
            file = 'case_file_day.csv',
            sep = ' ',quote = F,row.names = F)

#'
#' ## export by month ##
#'

#' This file contains a location ID, a number of cases and a date

df_cnt_day_lep <-
  readRDS('../../2_formatted_data/df_cnt_day_lep.rds')

df_cnt_month_lep <-
  df_cnt_day_lep %>%
  mutate(date_month=lubridate::floor_date(as.Date(day),unit = "month")) %>%
  group_by(GBcode_cnt,date_month) %>%
  summarize(count=sum(count,na.rm=T))

# change orders of columns for satscan
df_cnt_month_lep <- df_cnt_month_lep[,c('GBcode_cnt','count','date_month')]

str(df_cnt_month_lep)

write.table(x = df_cnt_month_lep,
            file = 'cnt_month/case_file_cnt_month.csv',
            sep = ' ',quote = F,row.names = F)

#'
#' ## export by year ##
#'

# load counts per township-year
df_twn_year_lep <-
  readRDS(file = '../../2_formatted_data/df_twn_year_lep.rds')

# change orders of columns for satscan
df_twn_year_lep <- 
  df_twn_year_lep[,c('TWN_CODE','count','year')]

df_missing <-
  df_twn_year_lep[which(df_twn_year_lep$TWN_CODE %in% TWN_CODE_missingPop),]
sum(df_missing$count) # that's ok

df_twn_year_lep <- 
  df_twn_year_lep[!(df_twn_year_lep$TWN_CODE %in% TWN_CODE_missingPop),]

write.table(
  x = df_twn_year_lep,
  file = 'case_file_year.csv',
  sep = ' ',quote = F,row.names = F)

#'
#' ## export by county and over study period ##
#'

# load counts per county
df_cnt_lep <-
  readRDS(file = '../../2_formatted_data/df_cnt_lep.rds')

nrow(df_cnt_lep);nrow(df_cnt_pop)

# change orders of columns for satscan
df_cnt_lep <- 
  df_cnt_lep[,c('CNTY_CODE','count')]

write.table(
  x = df_cnt_lep,
  file = 'case_file_cnt.csv',
  sep = ' ',quote = F,row.names = F)

#'
#' ## also export one file per year ##
#'

vect_years <- 2004:2014
for(i_year in 1:length(vect_years)){
  write.table(
    x = subset(df_twn_year_lep,
               subset = year == vect_years[i_year]),
    file = paste0('case_file_',vect_years[i_year],'.csv'),
    sep = ' ',quote = F,row.names = F)
}


#'
#' # Coordinates file #
#' 

#'
#' ## for townships ##
#'

# satscan uses long-lat
townships <- 
  sp::spTransform(x = townships,CRSobj = "+proj=longlat +datum=WGS84")

df_coordinates <- as.data.frame(coordinates(townships))
names(df_coordinates) <- c('long','lat')
df_coordinates$TWN_CODE <- townships$TWN_CODE
head(df_coordinates)

# get rid of townships with missing population
df_coordinates <- 
  df_coordinates[!(df_coordinates$TWN_CODE %in% TWN_CODE_missingPop),]

# map of coordinates to check
ggplot(df_coordinates) +
  geom_point(mapping=aes(x=long,y=lat,col=as.integer(TWN_CODE))) +
  coord_equal() +
  theme_bw()

write.table(x = df_coordinates,file = 'coordinates_file.csv',
            sep = ' ',quote = F,row.names = F)

#'
#' ## for counties ##
#'



sp_counties <- 
  readRDS(file = '../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')
proj4string(sp_counties)

# satscan uses long-lat
sp_counties_latlong <- 
  sp::spTransform(x = sp_counties,CRSobj = "+proj=longlat +datum=WGS84")

df_coordinates <- as.data.frame(coordinates(sp_counties_latlong))
names(df_coordinates) <- c('long','lat')
df_coordinates$GBcode_cnt <- sp_counties$GBcode_cnt
head(df_coordinates)

# map of coordinates to check
ggplot(df_coordinates) +
  geom_point(mapping=aes(x=long,y=lat,col=as.integer(GBcode_cnt))) +
  coord_equal() +
  theme_bw()

write.table(x = df_coordinates,file = 'coordinates_file_cnt.csv',
            sep = ' ',quote = F,row.names = F)

#' also export in utm coordinates (for ellipsoid analysis)
df_coordinates_utm <- as.data.frame(coordinates(sp_counties))
names(df_coordinates_utm) <- c('x_utm','y_utm')
df_coordinates_utm$GBcode_cnt <- sp_counties$GBcode_cnt
head(df_coordinates_utm)

head(df_coordinates)

# map of coordinates to check
ggplot(df_coordinates_utm) +
  geom_point(mapping=aes(x=x_utm,y=y_utm,col=as.integer(GBcode_cnt))) +
  coord_equal() +
  theme_bw()

write.table(x = df_coordinates_utm,
            file = 'coordinates_file_cnt_utm.csv',
            sep = ' ',quote = F,row.names = F)


#'
#' # Check: plot pop and cases by township #
#'

#'
#' change to dataframe
#'

townships@data$id <- rownames(townships@data)
df_townships <- fortify(townships,region="id")
df_townships <- 
  dplyr::left_join(x = df_townships,townships@data,by="id")

# join lepto data
df_townships <-
  dplyr::left_join(x = df_townships,
                   y = df_twn_year_lep,
                   by = )

ggplot(data = df_townships) +
  geom_polygon(mapping = aes(x=long,y=lat,group=group,
                             fill=pop)) +
  coord_fixed() + theme_bw()


