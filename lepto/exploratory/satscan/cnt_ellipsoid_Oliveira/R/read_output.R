#'
#'--- 
#'title: read output from satscan
#'author: Karina Cucchi
#'date: July 2 2018
#'---
#'

library(sp)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

name_simu = 'output'

#'
#' # Load data #
#'

# read sichuan shapefile

sp_sichuan <- 
  readRDS(file = '../../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')
proj4string(obj = sp_sichuan)

# read location information file (GIS)
df_gis <-
  foreign::read.dbf(file = paste0("../output/",name_simu,".gis.dbf"),
                    as.is = T)
str(df_gis)

#'
#' # Remove duplicated clusters #
#'

#' check whether one county can be in many clusters
length(unique(df_gis$LOC_ID))
nrow(df_gis)

df_gis <- 
  df_gis %>%
  subset(!duplicated(df_gis$LOC_ID))

nrow(df_gis)

#'
#' ## join tables ##
#'

sp_sichuan@data <-
  dplyr::left_join(x = sp_sichuan@data,
                   y = df_gis,
                   by = c("GBcode_cnt"="LOC_ID"))

#'
#' ##  plot clusters ## 
#' 



df_sichuan <- myUtils::sp2df(x_sp = sp_sichuan)
head(df_sichuan)


g_satscan_cnt <-
  ggplot(df_sichuan) +
  geom_polygon(mapping = aes(x=long,y=lat,group=group,
                             fill=factor(CLUSTER)),
               col='black') +
  scale_fill_manual(values = c("1"="black","2"="red"),
                    name="cluster\nindex") +
  coord_fixed() +
  theme_void()

g_satscan_cnt

# pdf(file = paste0('g_satscan_cnt_',name_simu,'.pdf'),width = 5,height = 5)
# print(g_satscan_cnt)
# dev.off()

#'
#' plot F function (clusters boundary uncertainty)
#'

g_satscan_cnt_f <-
  ggplot(df_sichuan) +
  geom_polygon(mapping = aes(x=long,y=lat,group=group,
                             fill=F_HIERARCH),
               col='black') +
  scale_fill_distiller(palette = "YlOrRd",
                       name="F-statistics",
                       direction=1) +
  coord_fixed() +
  theme_void()

g_satscan_cnt_f


pdf(file = paste0('g_satscan_cnt_',name_simu,'.pdf'),width = 8,height = 5)
print(cowplot::plot_grid(g_satscan_cnt +
                           theme(legend.position = "top"),
                         g_satscan_cnt_f +
                           theme(legend.position = "top")))
dev.off()


# check distribution of F statistics

g_F_stat <-
  ggplot(df_gis) +
  geom_histogram(mapping = aes(F_HIERARCH)) +
  scale_x_continuous(name = "Oliveira's F",
                     breaks = c(0.8,1)) +
  scale_y_continuous(name = "# counties",
                     breaks = c(0,1,10)) +
  theme_bw()

print(g_F_stat)

pdf(file = 'g_F_stat.pdf',width = 2.5,height = 1.5)
print(g_F_stat)
dev.off()

#'
#' # Export 2 cluster regions #
#'

#'
#' Keep only counties that have an F statistics of more than 0.8.
#'

str(sp_sichuan@data)

#'
#' define cluster North
#'

sp_cluster_north <- 
  sp_sichuan %>%
  subset(CLUSTER==2)


#' 
#' define cluster south
#' 

sp_cluster_south <- 
  sp_sichuan %>%
  subset(CLUSTER==1 & F_HIERARCH >=0.8)

#'
#' plot in same map
#'


plot(sp_sichuan)
plot(sp_cluster_north,col='red',add=T)
plot(sp_cluster_south,col='black',add=T)

#'
#' merge polygons
#'

sp_north_dissolved <-
  maptools::unionSpatialPolygons(SpP = sp_cluster_north,
                                 IDs = rep(1,nrow(sp_cluster_north)))

sp_south_dissolved <-
  maptools::unionSpatialPolygons(SpP = sp_cluster_south,
                                 IDs = rep(1,nrow(sp_cluster_south)))
proj4string(sp_south_dissolved)

#'
#' # Save newly defined regions #
#'


saveRDS(object = sp_north_dissolved,
        file = '../../sp_cluster_north.rds')

saveRDS(object = sp_south_dissolved,
        file = '../../sp_cluster_south.rds')

pdf(file = '../../clusters_north_south.pdf',width = 4,height = 4)
plot(sp_sichuan)
plot(sp_north_dissolved,col='red',add=T)
plot(sp_south_dissolved,col='black',add=T)
dev.off()

#'
#' save dataframe of counties and cluster they belong to (if relevant)
#'

df_sichuan_cluster <-
  sp_sichuan@data %>%
  select(GBcode_cnt,CLUSTER)

saveRDS(object = df_sichuan_cluster,file = '../../df_cluster_cnt.rds')
