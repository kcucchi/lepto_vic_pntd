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

name_simu = 'output_Oliveira'

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
  scale_fill_manual(values = c(rev(brewer.pal(n = 10,name = "Set3"))),
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
