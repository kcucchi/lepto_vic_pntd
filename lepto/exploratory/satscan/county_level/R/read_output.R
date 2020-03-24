#'
#'--- 
#'title: read output from satscan
#'author: Karina Cucchi
#'date: July 2 2018
#'---
#'


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

sp_output <-
  rgdal::readOGR(dsn = paste0('../',name_simu),
                 layer = 'output_spatial.col')
proj4string(sp_output)
# modify projection
sp_output <- spTransform(x = sp_output,CRSobj = proj4string(sp_sichuan))

plot(sp_sichuan,border='grey');plot(sp_output,add=T)

#'
#' plot counties instead
#' 

list_cnt <- 
  strsplit(x = readLines('../output/output_spatial_cntInClustersByHand.txt'),
           split = ", ")

df_cluster <- 
  data.frame(cluster=numeric(),GBcode_cnt=character(),
             stringsAsFactors = F)

for(i in 1:length(list_cnt)){
  
  df_cluster <-
    rbind(df_cluster,
          data.frame(cluster=i,
                     GBcode_cnt=list_cnt[[i]],
                     stringsAsFactors = F))
  
}

str(df_cluster)

setdiff(x = df_cluster$GBcode_cnt,y = sp_sichuan$GBcode_cnt)
duplicated(df_cluster$GBcode_cnt)
df_cluster <- df_cluster[!duplicated(df_cluster$GBcode_cnt),]
any(duplicated(df_cluster$GBcode_cnt))

nrow(sp_sichuan@data)

sp_sichuan@data <-
  dplyr::left_join(x = sp_sichuan@data,
                   y=df_cluster,
                   by="GBcode_cnt")
nrow(sp_sichuan@data)
sp_sichuan$cluster[is.na(sp_sichuan$cluster)] <- 0
head(sp_sichuan@data)


df_sichuan <- myUtils::sp2df(x_sp = sp_sichuan)

g_satscan_cnt <-
  ggplot(df_sichuan) +
  geom_polygon(mapping = aes(x=long,y=lat,group=group,
                             fill=factor(cluster)),
               col='black') +
  scale_fill_manual(values = c('white',rev(brewer.pal(n = 9,name = "Set3"))),
                    name="cluster\nindex") +
  coord_fixed() +
  theme_void()

g_satscan_cnt

pdf(file = 'g_satscan_cnt.pdf',width = 5,height = 5)
print(g_satscan_cnt)
dev.off()
