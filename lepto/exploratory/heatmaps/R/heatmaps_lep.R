#' ---
#' title: "Heatmaps"
#' author: "Karina Cucchi"
#' date: "March 8th, 2017"
#' ---

library(fields) # used here for color scheme
library(sp) # convenient format for spatial dataset
library(maptools) # for importing shapefiles
library(raster) # for rainfall raster data
library(dplyr) # dataframe manipulation
library(ggplot2)
library(RColorBrewer) # for color scales
library(rgeos) # for operations with polygons (eg merge)
library(rgdal) # for spTransform
library(lubridate) # for manipulating dates
library(dplyr)

#'
#'  # Load data #
#' 


#' ## Sichuan shapefile ##

sichuan <- 
  readRDS(file = '../../../data/1_clean_data/sichuan_adm/counties_ref.rds')

#'
#' # Week-year heatmaps #
#'

#'
#' ## heat map breaking down by year and week in year ##
#'

#'
#' prepare data
#'

# this dataframe contains counts of lepto cases by week
df_week_lep <- 
  readRDS(file = '../../../data/2_formatted_data/df_week_lep.rds')
str(df_week_lep)

# add field for year
df_week_lep$year <- lubridate::year(x = df_week_lep$sundays)
# add field for week in year (move all years to 2000)
df_week_lep$weekInYear <- as.Date(df_week_lep$sundays)
year(df_week_lep$weekInYear) <- 2000

#'
#' plot
#'

g_week_year <-
  ggplot(df_week_lep, aes(weekInYear,factor(year))) +
  geom_tile(aes(fill = count),width=7.5) +
  lims(y=as.character(2004:2014)) +
  scale_x_date(date_breaks = "month",date_labels = "%b") +
  labs(x="week in year",y="years",fill = "count") +
  scale_fill_gradientn(colours = 
                         colorRampPalette(rev(brewer.pal(11, "Spectral")),
                                          space="Lab")(100)) +
  theme_bw()

g_week_year

#'
#' ## scale by number of occurence in year ##
#'

#'
#' prepare data
#'

# load yearly data
df_year_lep <- 
  readRDS('../../../data/2_formatted_data/df_year_lep.rds')
names(df_year_lep) <- 
  gsub(pattern = 'count',replacement = 'countInYear',x = names(df_year_lep))
df_year_lep$year <- as.numeric(df_year_lep$year)

# join to weekly dataframe
df_week_lep <- 
  dplyr::left_join(x = df_week_lep,
                   y = df_year_lep,
                   by = "year")

#'
#' plot
#'

g_week_year_scaled <-
  ggplot(df_week_lep, aes(weekInYear,factor(year))) +
  geom_tile(aes(fill = count/countInYear),width=7.5) +
  lims(y=as.character(2004:2014)) +
  scale_x_date(date_breaks = "month",date_labels = "%b") +
  labs(x="week in year",y="years",fill = "relative\ncounts\nin year") +
  scale_fill_gradientn(colours = 
                         colorRampPalette(rev(brewer.pal(11, "Spectral")),
                                          space="Lab")(100)) +
  theme_bw()

g_week_year_scaled


# red colors when incidence peak very localized within year, 
# yellow colors when incidences more spread out throughout the year

#'
#' # year-latitude heatmaps #
#'


#'
#' ## prepare latitude fields ##
#'

sp_lep <- 
  readRDS(file = '../../../data/1_clean_data/sichuan_database/sp_lep.rds')

# get projection in lat-long
sp_lep_latLong <- spTransform(sp_lep,CRS("+proj=longlat"))


# check how latitudes are looking
hist(coordinates(sp_lep_latLong)[,2])

# define latitude breaks
lat_breaks <- pretty(coordinates(sp_lep_latLong)[,2],n = 10)
lat_mid <- ( lat_breaks[1:(length(lat_breaks)-1)] + 
               lat_breaks[2:length(lat_breaks)] ) / 2

# find each latitude category for each case point
sp_lep_latLong$lat_cat <- findInterval(x = coordinates(sp_lep_latLong)[,2],
                                       vec = lat_breaks)
sp_lep_latLong$lat_mid <- lat_mid[sp_lep_latLong$lat_cat]

sichuan_latlong <- spTransform(sichuan,CRS("+proj=longlat"))

b <- bbox(sp_lep_latLong)

g_lat_map <-
  ggplot() +
  geom_polygon(data = sichuan_latlong,
               aes(x=long, y = lat, group = group),
               fill="grey90",color='black')  +
  geom_hline(aes(yintercept=lat_breaks)) +
  coord_fixed(xlim=c(b[1,1],b[1,2]),
              ylim = c(b[2,1],b[2,2]),ratio = 1)  +
  labs(x='longitude',y='latitude') +
  theme(legend.position="none") +
  geom_point(data = as.data.frame(coordinates(sp_lep_latLong)),
             mapping = aes(x=x1,y=y1),alpha=1/5) +
  theme_bw()

g_lat_map

#'
#' ## heatmap by lat_cat and year ##
#'

#'
#' create dataframe with combinations of lat_mid and year
#'  

df_year_lat <- 
  expand.grid(year=unique(sp_lep_latLong@data$year),
              lat_mid=unique(sp_lep_latLong@data$lat_mid),
              stringsAsFactors = F)

df_year_lat <-
  dplyr::left_join(x = df_year_lat,
                   y = as.data.frame(sp_lep_latLong@data %>% 
                                       group_by(year,lat_mid) %>% 
                                       summarize(count=n())),
                   by = c("year", "lat_mid"))
df_year_lat$count[which(is.na(df_year_lat$count))] <- 0


#'
#' now plot
#'

g_year_lat <-
  ggplot(df_year_lat, aes(year,lat_mid)) +
  geom_tile(aes(fill = count)) +
  geom_text(aes(label = round(count, 1)),size=3) +
  scale_fill_gradientn(colours = 
                         colorRampPalette(rev(brewer.pal(11, "Spectral")),
                                          space="Lab")(100)) +
  labs(x="year",y="latitude",fill="count") +
  scale_y_continuous(breaks = lat_breaks) +
  theme_bw()

g_year_lat

#'
#' ## heatmap by lat_cat and week ##
#'

#'
#' prepare dataframe with combinations of lat_mid and year
#'

# read dataframe with all weeks
df_week_ref <-
  readRDS('../../../data/2_formatted_data/df_week_ref.rds')

df_week_lat <- 
  expand.grid(sundays=df_week_ref$sundays,
              lat_mid=unique(sp_lep_latLong@data$lat_mid),
              stringsAsFactors = F)

df_week_lat <-
  dplyr::left_join(x = df_week_lat,
                   y = as.data.frame(sp_lep_latLong@data %>% 
                                       group_by(sundays,lat_mid) %>% 
                                       summarize(count=n())),
                   by = c("sundays", "lat_mid"))
df_week_lat$count[which(is.na(df_week_lat$count))] <- 0

#'
#' now plot
#'

g_week_lat <-
  ggplot(df_week_lat, aes(as.Date(sundays),lat_mid)) +
  geom_tile(aes(fill = count)) +
  scale_fill_gradientn(colours = 
                         colorRampPalette(rev(brewer.pal(11, "Spectral")),
                                          space="Lab")(100)) +
  labs(x="year",y="latitude",fill="count") +
  scale_x_date(limits = as.Date(c("2004-01-01","2014-12-31")),
               date_breaks = "year",
               date_labels = "%Y") +
  scale_y_continuous(breaks = lat_breaks) +
  geom_vline(xintercept = as.Date(paste0(2004:2014,"-01-01")),
             linetype="dashed") +
  theme_bw()

g_week_lat

# check zoom on first 3 years
g_week_lat +
  xlim(as.Date("2004-01-01"),as.Date("2007-01-01"))



#' These heat maps seem to show that there is a propagation
#' of lepto incidence northwards.
#' This can be due to:
#' - the timing of the rice harvest
#' - changing temperature/precipitation conditions.

#' Check whether the magnitude of events are correlated to
#' magnitude of temperature/precipitation events?

#'
#' # Save counts by lat band and week #
#'

# (not ideal to do it here, but oh well)

saveRDS(object = df_week_lat ,file = 'df_week_lat.rds')

#'
#' # save all heatmaps generated in this file in a separate RData file #
#'

# these are the names of plot variables
g_names <- ls()[grep(pattern = '^g_',x = ls())]

# save in pdf
for(i in 1:length(g_names)){
  pdf(file = paste0('../',g_names[i],'.pdf'),
      width = 8,height = 5)
  print(get(g_names[i]))
  dev.off()
}

# save in RData file
save(list = g_names,file = 'heatmaps_lep.RData')

