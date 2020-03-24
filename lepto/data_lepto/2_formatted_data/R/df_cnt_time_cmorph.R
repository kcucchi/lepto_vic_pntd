#' ---
#' title: "Extracts rainfall data at the county level"
#' author: "Karina Cucchi"
#' date: "July 24th, 2016"
#' ---
#' 
#' In this script I load and format rainfall data 
#' for further exploration at county-level.
#' I extract rainfall at yearly and weekly resolution.
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

counties <- readRDS('../../1_clean_data/sichuan_adm/counties_ref.rds')
proj4string(counties)

#'
#' ## Load reference cnt_time dataframes ##
#'

df_cnt_year <- readRDS(file = '../df_cnt_year_ref.rds')
df_cnt_week <- readRDS(file = '../df_cnt_week_ref.rds')

#' 
#' ## Load rainfall satellite data ##
#' 

r <- readRDS('../../1_clean_data/sichuan_hydroDB/cmorph.rds')
proj4string(r)


#'
#' # Extract rainfall at the yearly resolution #
#'

# define dates corresponding to each layer
r_dates <- as.Date(names(r),format = "X%Y.%m.%d")

#'
#' ## 1. at yearly resolution ##
#'

#'
#' ### sum by year ###
#'

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

#'
#' ### extract ###
#'

# extract yearly rainfall at each county
cnt_cmorph_yearly <- 
  raster::extract(r_byYear, counties,
                  fun="mean", # mean over county
                  na.rm=TRUE,method="simple")
# cnt_cmorph_yearly is a matrix containing mean daily rainfall for each year
# each row is a county
rownames(cnt_cmorph_yearly) <- counties$CNTY_CODE
# each column is a year
colnames(cnt_cmorph_yearly) <- r_yearNames

#'
#' ### reshape ###
#'

cnt_cmorph_yearly_long <- reshape2::melt(data = cnt_cmorph_yearly)

# check output
str(cnt_cmorph_yearly_long)

# modify names of fields
names(cnt_cmorph_yearly_long) <- c("CNTY_CODE","year","cmorph_mmPday")
str(cnt_cmorph_yearly_long)

# modify to character
cnt_cmorph_yearly_long$CNTY_CODE <- 
  as.character(cnt_cmorph_yearly_long$CNTY_CODE)
cnt_cmorph_yearly_long$year <- 
  as.character(cnt_cmorph_yearly_long$year)
str(cnt_cmorph_yearly_long)

#'
#' ### join to reference dataframe ##
#'

df_cnt_year_cmorph <-
  dplyr::left_join(x = df_cnt_year,
                   y = cnt_cmorph_yearly_long,
                   by = c("year","CNTY_CODE"))

#'
#' ### save ###
#'

saveRDS(object = df_cnt_year_cmorph,
        file = '../df_cnt_year_cmorph.rds')


#'
#' ## 2. at weekly resolution ##
#'

#'
#' ### sum by week ###
#'

# r_dates contains dates for each layer in the raster
# calculate date for each preceding sunday with package lubridate
r_sundays <- lubridate::floor_date(x=r_dates,unit = "week")
# get index corresponding to year
r_sundaysIdx <- as.numeric(as.factor(r_sundays))
# get weeks corresponding to indices
r_sundaysNames <- levels(as.factor(r_sundays))

# actually take the mean layers by week
r_byWeek <- raster::stackApply(r, r_sundaysIdx, fun = mean)
names(r_byWeek) <- r_sundaysNames

#'
#' ### extract ###
#'

# extract weekly rainfall at each county
# this takes ~7s
cnt_cmorph_weekly <- 
  raster::extract(r_byWeek, counties,
                  fun="mean", # mean over county
                  na.rm=TRUE,method="simple")
# cnt_cmorph_weekly is a matrix containing
# mean weekly rainfall for each week
# each row is a county
rownames(cnt_cmorph_weekly) <- counties$CNTY_CODE
#each column is a week
colnames(cnt_cmorph_weekly) <- r_sundaysNames

#'
#' ### reshape ###
#'

cnt_cmorph_weekly_long <- reshape2::melt(data = cnt_cmorph_weekly)

# check output
str(cnt_cmorph_weekly_long)

# modify names of cnt_cmorph_weekly_long
names(cnt_cmorph_weekly_long) <- c("CNTY_CODE","sundays","cmorph_mmPday")
str(cnt_cmorph_weekly_long)

# modify from factor to character
cnt_cmorph_weekly_long$CNTY_CODE <- as.character(cnt_cmorph_weekly_long$CNTY_CODE)
cnt_cmorph_weekly_long$sundays <- as.character.factor(cnt_cmorph_weekly_long$sundays)
str(cnt_cmorph_weekly_long)

#'
#' ### join to reference dataframe ##
#'

df_cnt_week_cmorph <-
  dplyr::left_join(x = df_cnt_week,
                   y = cnt_cmorph_weekly_long,
                   by = c("sundays","CNTY_CODE"))
str(df_cnt_week_cmorph)

# setdiff(cnt_cmorph_weekly_long$sundays,df_cnt_week_cmorph$sundays)

#'
#' ### save ###
#'

saveRDS(object = df_cnt_week_cmorph,
        file = '../df_cnt_week_cmorph.rds')



# ############ old plots ###############
# 
# 
# #' ## plot rainfall per year ##
# 
# #' plot rainfall and lepto for one year with lepto data
# 
# # ggplot plots dataframe
# counties.df <- fortify(counties, region = "Object_ID_cnt")
# counties@data$Object_ID_cnt <- as.character(counties@data$Object_ID_cnt)
# 
# g_rYearly <-
#   rasterVis::gplot(r_byYear[[3]]) +
#   ggtitle(substr(names(r_byYear[[3]]),2,5)) +
#   geom_tile(aes(fill = value),color="darkblue") +
#   scale_fill_gradientn(colours = 
#                          colorRampPalette(brewer.pal(9, "YlGnBu"),
#                                           space="Lab")(100)) +
#   geom_polygon(data=counties.df,
#                aes(x = long, y = lat, group=group),
#                color="gray40",fill=NA,size=0.3) +
#   geom_point(aes(x=POINT_X,y=POINT_Y,alpha="leptospirosis\ncase"),
#              data = as.data.frame(coordinates(lep))) +
#   coord_equal() +
#     labs(x = "Longitude", y = "Latitude",
#        fill="mean\nrainfall\n[mm/day]",alpha="") +
#   # scale_fill_gradient(low = 'white', high = 'darkblue') +
#   xlim(2e5,9e5) +
#   blank() +
#   scalebar(counties.df, dist = 200,st.size = 5,st.bottom = F)  +
#   north(counties.df,symbol = 3,scale = 0.15) +
#   theme(panel.border = element_blank()) # removes the border from the plot
# 
# if(plotPdf) pdf('cnt_rainfall_r_2005.pdf',width=5,height = 6)
# g_rYearly
# if(plotPdf) dev.off()
# 
# # Now plot the northern cluster
# if(plotPdf) pdf('cnt_rainfall_r_2005_zoom.pdf',width=5,height = 6)
# g_rYearly + xlim(5.5e5,8e5) + ylim(3.35e6,3.6e6)
# if(plotPdf) dev.off()
# 
# #' first plot without lepto data
# plotByYear=F
# if(plotByYear){
#   for(i in 2004:2014){
#     plot(r_byYear[[paste0("X",i)]],legend=FALSE, axes=FALSE,
#          col=brewer.pal(9, "YlGnBu"),
#          xlim=c(2e5,8e5),ylim=c(3e6,3.6e6),
#          breaks=seq(min(minValue(r_byYear)),
#                     max(maxValue(r_byYear)),
#                     length.out=9))
#     plot(counties,add=T,axes=T,border='grey30')
#     # add legend
#     plot(r_byYear[[paste0("X",i)]], legend.only=TRUE,
#          legend.width=1, legend.shrink=0.75,
#          col=brewer.pal(9, "YlGnBu"),
#          breaks=seq(min(minValue(r_byYear)),
#                     max(maxValue(r_byYear)),
#                     length.out=9),
#          axis.args=list(at=pretty(values(r_byYear),n = 10),
#                         labels=pretty(values(r_byYear),n=10),
#                         cex.axis=0.6),
#          legend.args=list(text='rainfall [mm/day]',
#                           side=4, font=2, line=2.5, cex=0.8))
#     title(i)
#     plot(NOAAgauges_sichuan,pch=17,add=T,col='red')
#   }
# }
# 
# 
# # plot rainfall for years
# if(plotByYear){
#   for(i in 2004:2014){
#     plot(r_byYear[[paste0("X",i)]],legend=FALSE, axes=FALSE,
#          col=brewer.pal(8, "YlGnBu"),
#          xlim=c(2e5,8e5),ylim=c(3e6,3.6e6),
#          breaks=seq(min(minValue(r_byYear)),
#                     max(maxValue(r_byYear)),
#                     length.out=9))
#     plot(counties,add=T,axes=T,border='grey30')
#     # add legend
#     plot(r_byYear[[paste0("X",i)]], legend.only=TRUE,
#          legend.width=1, legend.shrink=0.75,
#          col=brewer.pal(8, "YlGnBu"),
#          breaks=seq(min(minValue(r_byYear)),
#                     max(maxValue(r_byYear)),
#                     length.out=9),
#          axis.args=list(at=pretty(values(r_byYear),n = 10),
#                         labels=pretty(values(r_byYear),n=10),
#                         cex.axis=0.6),
#          legend.args=list(text='rainfall [mm/day]',
#                           side=4, font=2, line=2.5, cex=0.8))
#     title(i)
#     # add lepto data
#     plot(lep[which(lep$year_diag==i),],add=T,pch=19,cex=0.5)
#   }
# }
# 
# 
# 
# #'
# #' ## Now extract yearly rainfall for each county  ##
# #'
# 
# #'
# #' # Aug-Oct rainfall extraction #
# #'
# #' In each county, extract total rainfall.
# 
# #'
# #' ## prepare for summation by Aug-Oct for each year ##
# #'
# 
# # first get indices of days out of the Aug-Oct period
# r_months <- as.numeric(format(r_dates,format="%m"))
# idx_discard <- which (r_months < 8 | r_months > 10)
# # now get index corresponding to years
# r_years <- as.numeric(format(r_dates,format="%Y"))
# r_yearIdx <- as.numeric(as.factor(r_years))
# # in year indices, replace indices outside of Aug-Oct range by NA
# r_yearIdx[idx_discard] <- NA
# # get years corresponding to indices
# r_yearNames <- levels(as.factor(r_years))
# 
# # actually take the mean by year
# r_byYear_AugOct <- raster::stackApply(r, r_yearIdx, fun = mean,na.rm = T)
# # get rid of the layer index_NA
# r_byYear_AugOct <- dropLayer(x = r_byYear_AugOct,
#                              i = which(names(r_byYear_AugOct) == "index_NA"))
# # rename layers
# names(r_byYear_AugOct) <- r_yearNames
# 
# #'
# #' ## Now extract Aug-Oct rainfall for each county  ##
# #'
# 
# # extract yearly rainfall at each county
# r_byCounty_AugOct <- extract(r_byYear_AugOct, counties,
#                              fun="mean",na.rm=TRUE,method="simple")
# # r_byCounty is a matrix containing mean daily rainfall for each year
# # each row is a county
# rownames(r_byCounty_AugOct) <- counties$Object_ID_cnt
# # each column is a year
# colnames(r_byCounty_AugOct) <- r_yearNames
# 
# # add in county dataframe
# counties$cmorph_AugOctmm <- r_byCounty_AugOct
# 
# #'
# #' # Also do number of days over the median over Aug-Oct #
# #'
# 
# unique_years <- unique(r_years)
# 
# # for each county, calculate median and 90th percentile of rainfall
# # in Aug-Oct over all years
# 
# # for that I need to extract timeseries of daily rainfall
# # corresponding to that period
# r_months <- as.numeric(format(r_dates,format="%m"))
# idx_inAugOct <- which( r_months >= 8 & r_months <= 10)
# r_inAugOct <- raster::subset(x = r,idx_inAugOct)
# 
# # now extract timeseries per county
# ts_cmorph_AugOct <-
#   raster::extract(x = r_inAugOct,y = counties,
#                   fun="mean", # perform mean over each county polygon
#                   na.rm=T)
# # for each county, calculate median and 90th percentile over all years
# counties$cmorph_median_AugOct <- numeric(nrow(counties))
# counties$cmorph_90perc_AugOct <- numeric(nrow(counties))
# for(i in 1:nrow(counties)){
#   counties$cmorph_median_AugOct[i] <-
#     median(ts_cmorph_AugOct[i,],na.rm = T)
#   counties$cmorph_90perc_AugOct[i] <-
#     quantile(ts_cmorph_AugOct[i,],0.9,na.rm = T)
# }
# hist(counties$cmorph_median_AugOct)
# hist(counties$cmorph_90perc_AugOct)
# 
# # now calculate the number of days above the median within each year
# counties$cmorph_daysOverMedian_AugOct <-
#   matrix(nrow=nrow(counties),ncol = length(unique_years))
# colnames(counties$cmorph_daysOverMedian_AugOct) <- unique_years
# 
# # now calculate the number of days above 90th percentile within each year
# counties$cmorph_daysOver90th_AugOct <-
#   matrix(nrow=nrow(counties),ncol = length(unique_years))
# colnames(counties$cmorph_daysOver90th_AugOct) <- unique_years
# 
# # get years corresponding to columns in ts_cmorph_AugOct
# colYears_ts_cmorph_AugOct <- as.numeric(
#   substr(x = colnames(ts_cmorph_AugOct),start = 2,stop = 5))
# 
# for(i in 1:nrow(counties)){ # loop over each county
#   # get median value for county i
#   med_i <- counties$cmorph_median_AugOct[i]
#   perc90th_i <- counties$cmorph_90perc_AugOct[i]
#   for(y in 1:length(unique_years)){ # loop over each year
#     # get the timeseries of daily rainfall
#     # for townwship i and year y
#     ts_cmorph_iy <-
#       ts_cmorph_AugOct[i,
#                        which(colYears_ts_cmorph_AugOct == unique_years[y])]
#     # get number of days over the median
#     counties$cmorph_daysOverMedian_AugOct[i,y] <-
#       sum (ts_cmorph_iy >= med_i ,na.rm = T)
#     # get number of days over the 90th percentile
#     counties$cmorph_daysOver90th_AugOct[i,y] <-
#       sum (ts_cmorph_iy >= perc90th_i ,na.rm = T)
#   }
# }
# hist(counties$cmorph_daysOverMedian_AugOct)
# hist(counties$cmorph_daysOver90th_AugOct)


