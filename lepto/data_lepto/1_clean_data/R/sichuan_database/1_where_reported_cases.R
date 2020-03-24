#'
#' In this script I try to better understand how cases locations are reported.
#' To make decisions on what to use
#'

library(sp)
library(maptools)

source('../../../refs.R')

#' # Load data #

counties <- readRDS('../../sichuan_adm/counties_ref.rds')

#' ## Load lepto data ##

#' lepto data from utm file
lep <- readRDS(file = 'sp_lep.rds')

#' lepto data as exported from gdb
lep_gdb_1 <- readRDS(file = 'sp_lep_gdb_1.rds')
lep_gdb_2 <- readRDS(file = 'sp_lep_gdb_2.rds')

#' # plot locations in both datasets #

plot(counties)
points(lep,pch=19)
points(lep_gdb_1,pch=3,col='red')
points(lep_gdb_2,pch=2,col="blue")

# zoom on subregion

plot(counties,col='grey',axes=T,
     xlim=c(6.2e5,6.7e5),ylim=c(3.45e6,3.5e6))
# plot(townships,add=T)
points(lep,pch=19)
points(lep_gdb_1,pch=3,col='red')
points(lep_gdb_2,pch=2,col="blue")

#' ## comparison between point locations ##

nrow(lep)
nrow(lep_gdb_1)
nrow(lep_gdb_2)

#'
#' Looks like x2,y2 have more refined locations.
#' But there are a lot of missing data points
#' (only 65% percent of the entire dataset).
#'

#' 
#' # Analysis of number of reported cases per location #
#' 

# check individual locations of lepto incidence
unique_loc <- unique(coordinates(lep))

# this vector will contain times of incidence diagnosis at each location
ts_loc <- list(length=nrow(unique_loc))

for(i_loc in 1:nrow(unique_loc)){
  
  # find indices of rows corresponding to location unique_loc[i_loc,]
  idx_loc <- which(apply(coordinates(lep), 1, function(x) all(x == unique_loc[i_loc,])))
  
  # get corresponding diagnosis date
  ts_loc[[i_loc]] <- lep[['date_diagn']][idx_loc]
}

#' We can now check locations of higher diagnosis rates
#' 
#' The following plot shows how many incidence cases were reported per unique location. 
#' Most locations come with less than 5 incidence points, 
#' this gives confidence in the fact that we can model this data 
#' using a point process model (eg. POisson).

library(plyr)
lengths_perLoc <- unlist(lapply(ts_loc, length))
tab_lengths_perLoc <- table(lengths_perLoc)

barplot(tab_lengths_perLoc,
        xlab=expression('n'['i']*' - number of reported cases at one location'),
        ylab=expression('number of locations reporting n'['i']*' cases'))
title('number of reported cases by location')

#'
#' How many data points would there be if we discarded data with more than 5 occurences?
#' 

nbLoc_pernbCases <- cbind(1:length(tab_lengths_perLoc),
                          as.numeric(tab_lengths_perLoc))

cases_cumsum = rep(0,nrow(nbLoc_pernbCases))
accu = 0
for (i in 1:nrow(nbLoc_pernbCases)){
  accu <- accu + i*nbLoc_pernbCases[i,2]
  cases_cumsum[i] <- accu
}
plot(cases_cumsum,pch=19,ylim=c(0,3000),
     xlab=expression('n'['i']*'number of cases reported at one location'),
     ylab=expression('cumulative number of cases from regions with n'['i']*' cases'))
abline(v=pretty(1:nrow(nbLoc_pernbCases)),col='grey',lty=2)
abline(h=pretty(cases_cumsum),col='grey',lty=2)

#
#' # Check how many cases are reported at township centroids #
#' 
#' 

#' 
#' ## Import centroids from Sophie ##
#' 

# library(foreign)
# twn_centroids <- read.dbf('../../sichuan_database/twnshps_centroids/sichuancentroids.dbf')
# # transform to SpatialPointsDataFrame
# coordinates(twn_centroids) <- c('Longitude','Latitude')
# 
# plot(sichuan,col='grey',axes=T)
# plot(twn_centroids,add=T)

# oops, they do not overlap... projection issue?


#' 
#' ## Import centroids from database ##
#' 
library(rgdal)
townships <- rgdal::readOGR(dsn = '../../../0_raw_data/sichuan_database/from_gdb',
                            layer = 'Townships')
townships <- spTransform(x = townships,CRSobj = ref_CRS)


plot(townships,col='grey',axes=T)

plot(counties,col='grey',axes=T)
# get centroids of townships
twn_centroids <- coordinates(townships)
# transform numeric to dataframe
twn_centroids <- data.frame(twn_centroids)
names(twn_centroids) <- c('long','lat')
# transform to SpatialPointsDataFrame
coordinates(twn_centroids) <- c('long','lat')
plot(twn_centroids,pch=20,add=T)
proj4string(twn_centroids) <- ref_CRS

#' 
#' ## Overlaps - visual checks ##
#' 

#' Check for eg region 1 if it looks like they overlap the data

#' Color points by number of values at each location
#' 
#' 

# unique_loc contains the list of unique locations
# lengths_perLoc contains number of points at each location

# transform unique_loc to spatial points dataframe (easier for plotting)
unique_loc <- as.data.frame(unique_loc)
coordinates(unique_loc) <- c('POINT_X','POINT_Y')
unique_loc$lengthsPerLoc <- lengths_perLoc

# define colors corresponding to number of points per location
# change the colorbar not to confuse with years of incidence
pal <- rev(heat.colors(max(unique_loc$lengthsPerLoc)))
# plot(1:length(pal),pch=19,col=pal)
intcols_lengthPerLoc <- pal[unique_loc$lengthsPerLoc]

layout(matrix(c(1,2),nrow=1), widths=c(4,1))
par(mar=c(2,2,1,1))
plot(counties,col='grey',axes=T,
     xlim=c(6.2e5,6.7e5),ylim=c(3.45e6,3.5e6))
plot(townships,add=T)
plot(unique_loc,pch = 21,add=T,
     bg = intcols_lengthPerLoc,col='black')
plot(twn_centroids,pch=3,add=T)
par(mar=c(2,2,2,3))
# color.bar(pal,
#           min=min(unique_loc$lengthsPerLoc),
#           max=max(unique_loc$lengthsPerLoc),
#           title = '# incidence')

#' It looks like locations where incidence is reported usually are unique within a township, 
#' and that does not depend on the number of incidence points found at that location.

#' 
#' ## Check wrt to locations with high number of incidence reported ##
#' 


#' Check for region where largest number of reported cases 
#' 
#' Red crosses indicates that these locations are amongst the 10 locations reporting the most cases (from 26 to 39 cases reported at one location).

# vector if indices corresponding to 10 largest numbers of reported cases
idx_lengthest = match(sort(lengths_perLoc,decreasing = T)[1:10],
                      lengths_perLoc)

# now get corresponding location
loc_lengthest <- unique_loc[idx_lengthest,]

plot(counties)
points(loc_lengthest,pch=19,
       col=colorRampPalette(c("red", "white"))( 10 ),cex=0.5)

intcols <- pal[lep$year_diag - 2003]

plot(counties,col='grey',axes=T,
     xlim=c(6.2e5,6.8e5),ylim=c(3.45e6,3.5e6))
plot(lep, col = intcols, pch = 19,add=T,cex=0.5)
plot(twn_centroids,pch=3,add=3)
points(loc_lengthest,pch=3,col=colorRampPalette(c("red", "white"))( 10 ) ,cex=2)

#' Red crosses indicates that these locations are amongst the 10 locations
#' reporting the most cases (from 26 to 39 cases reported at one location).
#' 
#' Hard to discard whether they are actually located at township centroids...

#' 
#' ## Overlaps - compute number of overlapping data with buffers ##
#' 

#' First create buffer around township centroids

# 
library(rgeos)
townships_buffer <- gBuffer(twn_centroids, width=500, byid=TRUE )

# # plot buffers on top of map
# plot(sichuan,col='grey',axes=T,
#      xlim=c(6.2e5,6.8e5),ylim=c(3.46e6,3.49e6))
# plot(lep, col = intcols, pch = 19,add=T,cex=0.5)
# plot(townships,add=T)
# plot(twn_centroids,pch=3,add=3)
# points(loc_lengthest,pch=3,col=colorRampPalette(c("red", "white"))( 10 ) ,cex=2)
# plot(townships_buffer,add=T)

#' Now check intersection between lep data and buffers around township centers 

# this takes the intersection
lep_inBuffers <- lep[townships_buffer,]

paste(nrow(lep_inBuffers),'/',nrow(lep))

#' Seems that not many points are located at township centroids... Check where they are

library(rgeos)
townships_buffer <- gBuffer( twn_centroids, width=500, byid=TRUE )
plot(counties,col='grey',axes=T,
     xlim=c(6.2e5,6.8e5),ylim=c(3.46e6,3.49e6))
plot(lep, col = intcols, pch = 19,add=T,cex=0.5)
plot(townships,add=T)
plot(twn_centroids,pch=3,add=3)
points(loc_lengthest,pch=3,col=colorRampPalette(c("red", "white"))( 10 ) ,cex=2)
plot(townships_buffer,add=T)
plot(lep_inBuffers,add=T,pch=19)

#' And they also come with locations with high point density, 
#' meaning that there is a higher chance that some incidence points 
#' are located close to township centroids...

#' Or maybe township centroids computed here from township shapefile
#' are not the ones used when cases are reported at centroid locations ? 
#' 
#' Conclusion on township vs point-level modeling : the conclusions shouldn't be very different, as most of the time there is one location per township. For now, township level analysis is enough, with covariates extracted at township level.
#' 
#' At the scale of graph shown previously, it is possible that points show patterns of regularity. I could test for that using the F function, not sure if that's useful here so I'll stop here.
#' 
#' 
#' 
#' #
#' # Number of cases per township #
#' 
#' Howard said that our data is very sparse, with many township coming with no data points. Here I investigate the number of cases found in each township.
#' 

# Histogram of number of cases found per township

library(GISTools)
# this vector contains the number of points in each township
ptsPerTwnshp <- poly.counts(lep,townships)

# count how many township contain n incidence points
tab_ptsPerTwnshp <- table(ptsPerTwnshp)

# this is the percentage of township with no data at all.
tab_ptsPerTwnshp[1]/nrow(townships) * 100

# plot with cut y-axis
# install.packages('plotrix', dependencies = TRUE)
library(plotrix)
ylocs = c(seq(from=0,to=400,by=100),3850,3950)
gap.barplot(y=tab_ptsPerTwnshp,
            ytics = ylocs,yaxlab = ylocs,
            xaxlab = names(tab_ptsPerTwnshp),
            gap=c(401,3800),
            xlab = 'number of incidence points in township',
            ylab = 'number of townships')

#' Most townships come with no incidence point...
#' Confirmation of very sparse dataset.
#' 
#' # Conclusion #
#' 
#' use sp_lep_gdb1 : contains most cases. 
