#'
#' In this script I analyze the cases per basins.
#' 
#'

library(sp)
library(maptools)
library(ggplot2)
library(ggsn)

#' # Load data #
#' 
#' ## lepto data ##

# lepto spatial data
lep <- readRDS(file = '../../data/1_clean_data/sichuan_database/sp_lep.rds')
names(lep)

# lepto by basin
df_basins_lep <- readRDS('../../data/2_formatted_data/df_basins_lep.rds')
names(df_basins_lep)

#' ## basins ##

basins <-
  readRDS('../../data/1_clean_data/sichuan_hydroDB/basins_waterbase.rds')

plot(basins)
plot(lep,add=T)

#'
#' # Format data #
#'

# ggplot plots dataframe
basins@data$id <- rownames(basins@data)
basins.df <- fortify(basins,region="id")
basins.df <- dplyr::left_join(basins.df, basins@data, by="id")

# merge the cnt-lepto data with the county geometry
basins.df <- dplyr::left_join(x = basins.df,
                              y = df_basins_lep,
                              by = "BASINID")
str(basins.df)

basins.df$count_farmers[basins.df$count_farmers==0] <- NA

#'
#' # plots #
#'

g_basins_lep <-
  ggplot() +
  geom_polygon(data = basins.df,
               aes(x = long, y = lat, group=group,
                   fill=count_farmers)) +  
  geom_path(data = basins.df,aes(x = long, y = lat, group=group),
            colour="black", lwd=0.001) + # county borders
  coord_equal() +  
  scale_fill_gradient2(low = "white", high = "darkblue", # colors
                       trans="log10",
                       breaks = c(1,5,10,50,100,500)) +
  labs(x = "Longitude", y = "Latitude",
       fill = "counts\nby basin") +
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) + # remove axis ticks
  scalebar(basins.df, dist = 200,st.size = 4,st.bottom = F)  +
  north(basins.df,symbol = 3,scale = 0.15)

g_basins_lep


pdf('../g_basins_lep.pdf',width=5,height = 5)
g_basins_lep
dev.off()

pdf('../g_basins_lep_withLep.pdf',width=5,height = 5)
g_basins_lep +
  geom_point(data =  as.data.frame(coordinates(lep)),
             aes(x=x1,y=y1,alpha="leptospirosis\ncase"),
             size=0.5)
dev.off()  

#'
#' # Save plot #
#'

saveRDS(g_basins_lep,file = 'g_basins_lep.rds')
