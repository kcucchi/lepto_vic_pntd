#' ---
#' title: "Define shapefiles for Northern and Southern regions"
#' author: "Karina Cucchi"
#' date: "June 26th 2018"
#' ---

library(ggplot2)
library(sp)
library(maptools)

#'
#' # Load data #
#'

# read reference county shapefile

sp_cnt <- 
  readRDS(file = '../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')
head(sp_cnt@data)
proj4string(obj = sp_cnt)

# read count of lepto by county

df_cnt_lep <- readRDS(file = '../df_cnt_lep.rds')
head(df_cnt_lep)

#'
#' # Join data sets #
#'

sp_cnt@data <-
  dplyr::left_join(x = sp_cnt@data,
                   y = df_cnt_lep[,c('CNTY_CODE','cntWithLep')],
                   by = c("GBcode_cnt"="CNTY_CODE"))

#'
#' # Define regions
#'

sp_cnt <- spTransform(sp_cnt,CRS("+proj=longlat"))

# define centroids
df_cnt_centroids <- coordinates(sp_cnt)

def_x_0 = 105
def_y_0 = 30.4
def_slope = -0.3
#def of the boundary is 
# y = def_slope * x + (def_y_0-def_slope*def_x_0) >= 0

sp_cnt$isNorth <- 
  df_cnt_centroids[,2] >= def_slope * df_cnt_centroids[,1] +
  (def_y_0-def_slope*def_x_0) 

#' define and merge

sp_north <- 
  sp_cnt[sp_cnt$isNorth & sp_cnt$cntWithLep,]
sp_north <- 
  unionSpatialPolygons(SpP = sp_north,
                       IDs = rep(1,length(sp_north)))

sp_south <- 
  sp_cnt[!sp_cnt$isNorth & sp_cnt$cntWithLep,c("GBcode_cnt","ename")]
sp_south <- 
  unionSpatialPolygons(SpP = sp_south,
                       IDs = rep(1,length(sp_south)))


#'
#' # Export data #
#'

# save geographic regions
saveRDS(object = sp_north,
        file = '../sp_north.rds')
saveRDS(object = sp_south,
        file = '../sp_south.rds')

# save dataframe of counties and whether north or south
saveRDS(object = sp_cnt@data,
        file = '../df_cnt_north_south.rds')

#'
#' # Plot #
#'

df_cnt <- 
  myUtils::sp2df(x_sp = sp_cnt)
head(df_cnt)

#'
#' plot with counties
#'

g_NorthSouth_cnt <-
  ggplot(df_cnt) +
  geom_polygon(mapping = aes(x=long,y = lat,group=group,
                             fill=cntWithLep,col=isNorth)) +
  geom_abline(mapping = aes(slope=def_slope,
                            intercept=def_y_0-def_slope*def_x_0)) +
  scale_color_manual(values = c("TRUE"="red","FALSE"="black"),
                     name="",
                     labels=c("TRUE"="North","FALSE"="South")) +
  scale_fill_manual(values = c("TRUE"="grey30","FALSE"="grey90"),
                    name="",
                    labels=c("TRUE"="at least one case","FALSE"="no cases")) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "bottom")

g_NorthSouth_cnt

pdf(file = 'g_NorthSouth_cnt.pdf',width = 5,height = 4)
print(g_NorthSouth_cnt)
dev.off()


#'
#' plot merged
#'

pdf(file = 'g_NorthSouth_merged.pdf',width = 5,height = 4)
plot(sp_cnt,border="grey40")
plot(sp_north,add=T,col="red",border=NA)
plot(sp_south,add=T,col="black",border=NA)
dev.off()


