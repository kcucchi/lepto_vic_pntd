#'
#' In this script I analyze the cases per prefecture.
#' 
#'

library(sp)
library(maptools)
library(ggplot2)
library(ggsn)
# source('../utils.R')

plotPdf = F

#' # Load data #
#' 
#' ## lepto data ##

#' lepto data from utm file
sp_lep <- 
  readRDS(file = '../../../data/1_clean_data/sichuan_database/sp_lep.rds')

#' ## prefectures ##

sp_pref <- 
  readRDS(file = '../../../data/1_clean_data/sichuan_adm/prefectures_ref.rds')
proj4string(sp_pref)
sp_pref <- spTransform(sp_pref, proj4string(sp_lep))

# check how many prefectures
nrow(sp_pref)

plot(sp_pref)
plot(sp_lep,add=T)


#'
#' # Count #
#'

sp_pref$lep_counts <- 
  GISTools::poly.counts(pts = sp_lep,polys = sp_pref)

sp_pref@data[order(sp_pref@data$lep_counts,decreasing = T),]

#'
#' # save prefecture object with counts #
#'

# saveRDS(prefectures,file = '../RData/prefectures_lepCounts.rds')

#'
#' # plots #
#'

sp_pref$lep_counts[sp_pref$lep_counts==0] <- NA

# ggplot plots dataframe
df_pref <- myUtils::sp2df(x_sp = sp_pref)

# for adding prefecture names on top of polygons
sp_pref$long <- coordinates(sp_pref)[,1]
sp_pref$lat <- coordinates(sp_pref)[,2]

# add special character for "Ngawa Tibetan and Qiang"
sp_pref$name[which(sp_pref$name == "Ngawa Tibetan and Qiang")] <-
  "Ngawa Tibetan\nand Qiang"

g_pref_lep <-
  ggplot() +
  geom_polygon(data = df_pref,
               aes(x = long, y = lat, 
                   group=group,fill=lep_counts),
               col = "grey") +  
  coord_equal() +
  scale_fill_gradient(low = "white", high = "red",
                      breaks = c(1,200,400,600),
                      limits=c(0,max(sp_pref$lep_counts,na.rm = T))) +
  labs(x = "Longitude", y = "Latitude",
       fill = "count by\nprefecture") +
  geom_text(data=sp_pref@data, #add labels at centroids
            aes(label = name,
                x = long,
                y = lat),
            size = 3) +
  theme_void() +
  theme(legend.position = "bottom") +
  scalebar(df_pref, dist = 200,st.size = 4,st.bottom = F)  +
  north(df_pref,symbol = 3,scale = 0.15)

g_pref_lep

pdf('../g_pref_lep.pdf',width=6,height = 5)
g_pref_lep
dev.off()
