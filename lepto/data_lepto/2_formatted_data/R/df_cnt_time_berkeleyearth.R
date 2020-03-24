#' ---
#' title: "Extracts temperature data at the county level"
#' author: "Karina Cucchi"
#' date: "July 24th, 2017"
#' ---
#' 
#' In this script I load and format temperature data 
#' for further exploration of relationship between lepto and env data.
#' I extract yearly and weekly means.
#' 

## load packages
library(raster) # for rasters
library(sp) # for spatial objects
library(lubridate) # for date manipulation
library(dplyr) # for dataframe manipulation


# plotPdf=F

#'
#' # First load relevant data #
#'

#'
#' ## Load counties data ##
#'

counties <- readRDS('../../1_clean_data/sichuan_adm/counties_ref.rds')
proj4string(counties)

#'
#' ## Load reference cnt_time dataframes ##
#'

df_cnt_year <- readRDS(file = '../df_cnt_year_ref.rds')
df_cnt_week <- readRDS(file = '../df_cnt_week_ref.rds')

#' 
#' ## Load temperature data from Berkeley earth ##
#' 

t <- readRDS(file = '../../1_clean_data/sichuan_tempDB/temp_berkeleyearth.rds')
proj4string(t)


# plot counties and raster on top of each other
plot(t[[5]])
plot(counties,add=T,axes=T)

#'
#' # Extract temperature #
#'

# define dates corresponding to each layer
t_dates <- as.Date(names(t),format = "X%Y.%m.%d")

#'
#' ## 1. at weekly resolution ##
#'

#'
#' ## sum by week ##
#'  

# r_dates contains dates for each layer in the raster
# calculate date for each preceding sunday with package lubridate
t_sundays <- lubridate::floor_date(x=t_dates,unit = "week")
# get index corresponding to week
t_sundaysIdx <- as.numeric(as.factor(t_sundays))
# get weeks corresponding to indices
t_sundaysNames <- levels(as.factor(t_sundays))

# actually take the mean layers by week
t_byWeek <- raster::stackApply(t, t_sundaysIdx, fun = mean)
names(t_byWeek) <- t_sundaysNames

# t_byWeek is a raster with one raster per week

#'
#' ### extract ###
#'

# extract weekly temperature at each county
# this takes ~7s
cnt_berkeleyearth_weekly <- raster::extract(
  t_byWeek, counties,
  fun="mean", # mean over township
  na.rm=TRUE,method="simple")
# cnt_berkeleyearth_weekly is a matrix containing
# mean temperature for each week
# each row is a county
rownames(cnt_berkeleyearth_weekly) <- counties$CNTY_CODE
#each column is a week
colnames(cnt_berkeleyearth_weekly) <- t_sundaysNames

#'
#' ### reshape ###
#'

cnt_berkeleyearth_weekly_long <-
  reshape2::melt(data = cnt_berkeleyearth_weekly)

# check output
str(cnt_berkeleyearth_weekly_long)

# modify names of cnt_berkeleyearth_weekly_long
names(cnt_berkeleyearth_weekly_long) <- c("CNTY_CODE","sundays","temp_C")
str(cnt_berkeleyearth_weekly_long)

# modify from factor to character
cnt_berkeleyearth_weekly_long$CNTY_CODE <- 
  as.character(cnt_berkeleyearth_weekly_long$CNTY_CODE)
cnt_berkeleyearth_weekly_long$sundays <- 
  as.character.factor(cnt_berkeleyearth_weekly_long$sundays)
str(cnt_berkeleyearth_weekly_long)

#'
#' ### join to reference dataframe ###
#'

df_cnt_week_berkeleyearth <-
  dplyr::left_join(x = df_cnt_week,
                   y = cnt_berkeleyearth_weekly_long,
                   by = c("sundays","CNTY_CODE"))
str(df_cnt_week_berkeleyearth)

#'
#' ### Save final variable ###
#'

saveRDS(object = df_cnt_week_berkeleyearth,
        file = "../df_cnt_week_berkeleyearth.rds")

#'
#' ## 2. at yearly resolution ##
#'

#'
#' ## add new fields ##
#'


df_cnt_week_berkeleyearth$sundays_date <- 
  as.Date(df_cnt_week_berkeleyearth$sundays)
df_cnt_week_berkeleyearth$year <- 
  as.character(lubridate::year(df_cnt_week_berkeleyearth$sundays_date))
df_cnt_week_berkeleyearth$month <- 
  as.character(lubridate::month(df_cnt_week_berkeleyearth$sundays_date))

#'
#' ### do mean over entire year ###
#'

df_cnt_year_temp <- 
  dplyr::summarise(group_by(df_cnt_week_berkeleyearth, year,CNTY_CODE),
                   temp_C_JanDec=mean(temp_C))

# join to reference dataframe
df_cnt_year <-
  dplyr::left_join(x = df_cnt_year,
                   y = df_cnt_year_temp,
                   by = c("year","CNTY_CODE"))

#'
#' ### do mean over rainy period only (May to October) ###
#'

# consider only a subset of the weekly dataset
df_cnt_year_temp <- 
  dplyr::summarise(group_by(subset(df_cnt_week_berkeleyearth,month %in% 5:10), 
                            year,CNTY_CODE),
                   temp_C_MayOct=mean(temp_C))

# join to reference dataframe
df_cnt_year <-
  dplyr::left_join(x = df_cnt_year,
                   y = df_cnt_year_temp,
                   by = c("year","CNTY_CODE"))

#'
#' ### do mean over lepto period only (August to October) ###
#'

# consider only a subset of the weekly dataset
df_cnt_year_temp <- 
  dplyr::summarise(group_by(subset(df_cnt_week_berkeleyearth,month %in% 8:10), 
                            year,CNTY_CODE),
                   temp_C_AugOct=mean(temp_C))

# join to reference dataframe
df_cnt_year <-
  dplyr::left_join(x = df_cnt_year,
                   y = df_cnt_year_temp,
                   by = c("year","CNTY_CODE"))

str(df_cnt_year)

#'
#' ### do mean from Jan to start of lepto period (January to July) ###
#'

# consider only a subset of the weekly dataset
df_cnt_year_temp <- 
  dplyr::summarise(group_by(subset(df_cnt_week_berkeleyearth,month %in% 1:7), 
                            year,CNTY_CODE),
                   temp_C_JanJul=mean(temp_C))

# join to reference dataframe
df_cnt_year <-
  dplyr::left_join(x = df_cnt_year,
                   y = df_cnt_year_temp,
                   by = c("year","CNTY_CODE"))


#'
#' ### do mean from Jan to end of lepto period (January to Oct) ###
#'

# consider only a subset of the weekly dataset
df_cnt_year_temp <- 
  dplyr::summarise(group_by(subset(df_cnt_week_berkeleyearth,month %in% 1:10), 
                            year,CNTY_CODE),
                   temp_C_JanOct=mean(temp_C))

# join to reference dataframe
df_cnt_year <-
  dplyr::left_join(x = df_cnt_year,
                   y = df_cnt_year_temp,
                   by = c("year","CNTY_CODE"))

str(df_cnt_year)

#'
#' ### save ###
#'

saveRDS(object = df_cnt_year,
        file = '../df_cnt_year_berkeleyearth.rds')






# ############ old plots ###############

# plot(t_byYear)
# 
# # ggplot plots dataframe
# counties.df <- fortify(counties, region = "Object_ID_cnt")
# counties@data$Object_ID_cnt <- as.character(counties@data$Object_ID_cnt)
# 
# g_tYearly <-
#   rasterVis::gplot(t_byYear[[6]]) +
#   ggtitle(substr(names(t_byYear[[6]]),2,5)) +
#   geom_tile(aes(fill = value),color="darkred") +
#   scale_fill_gradientn(colours = 
#                          colorRampPalette(brewer.pal(9, "YlOrRd"),
#                                           space="Lab")(100)) +
#   geom_polygon(data=counties.df,
#                aes(x = long, y = lat, group=group),
#                color="gray40",fill=NA,size=0.3) +
#   geom_point(aes(x=POINT_X,y=POINT_Y,alpha="leptospirosis\ncase"),
#              data = as.data.frame(coordinates(lep))) +
#   coord_equal() +
#   labs(x = "Longitude", y = "Latitude",
#        fill="mean\ntemperature\n[C]",alpha="") +
#   # scale_fill_gradient(low = 'white', high = 'darkblue') +
#   xlim(2e5,9e5) +
#   blank() +
#   scalebar(counties.df, dist = 200,st.size = 5,st.bottom = F)  +
#   north(counties.df,symbol = 3,scale = 0.15) +
#   theme(panel.border = element_blank()) # removes the border from the plot
# 
# if(plotPdf) pdf('cnt_temp_berkeleyearth_2005.pdf',width=5,height = 6)
# g_tYearly
# if(plotPdf) dev.off()

# # Now plot the northern cluster
# if(plotPdf) pdf('cnt_temp_berkeleyearth_2005_zoom.pdf',width=5,height = 6)
# g_tYearly + xlim(5.5e5,8e5) + ylim(3.35e6,3.6e6)
# if(plotPdf) dev.off()

# #'
# #' ## Now extract information for each township  ##
# #'


# #'
# #' # 2. extract mean temperature during Aug-Oct #
# #'

# # prepare for summation by Aug-Oct for each year
# # first get indices of days out of the Aug-Oct period
# t_months <- as.numeric(format(t_dates,format="%m"))
# idx_discard <-which (t_months < 8 | t_months > 10)
# # now get index corresponding to years
# t_years <- as.numeric(format(t_dates,format="%Y"))
# t_yearIdx <- as.numeric(as.factor(t_years))
# # in year indices, replace indices outside of Aug-Oct range by NA
# t_yearIdx[idx_discard] <- NA
# # get years corresponding to indices
# t_yearNames <- levels(as.factor(t_years))
# 
# # actually take the mean by year
# t_byYear_AugOct <- raster::stackApply(t, t_yearIdx, fun = mean,na.rm = T)
# # get rid of the layer index_NA
# t_byYear_AugOct <- dropLayer(x = t_byYear_AugOct,
#                              i = which(names(t_byYear_AugOct) == "index_NA"))
# # 
# names(t_byYear_AugOct) <- t_yearNames


# #'
# #' ## Now extract information for each township  ##
# #'

# # extract yearly rainfall at each township
# t_byCounty_AugOct <- extract(t_byYear_AugOct, counties,
#                              fun="mean",na.rm=TRUE,method="simple")
# # t_byCounty is a matrix containing mean 
# # Aug-Oct daily temperature for each year
# # each row is a township, each column is a year
# colnames(t_byCounty_AugOct) <- t_yearNames
# 
# # drop years 2000 to 2003 (outside of lep dataset)
# t_byCounty_AugOct <- t_byCounty_AugOct[,as.character(2004:2013)]
# # add empty column for year 2014 (no data)
# t_byCounty_AugOct <- 
#   cbind(t_byCounty_AugOct,"2014"=rep(NA,nrow(t_byCounty_AugOct)))
# 
# rownames(t_byCounty_AugOct) <- counties$Object_ID_cnt
# 
# # add in township dataframe
# counties$temp_berkeleyearth_meanAugOct <- t_byCounty_AugOct



