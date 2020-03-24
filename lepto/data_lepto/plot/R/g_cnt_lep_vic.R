#'
#' # Plot overlay of lepto and vic varaiables#
#'

library(ggplot2)
library(ggsn)
library(sp)
library(scales) # for muted colors


var = 'roff'

#'
#' # Load data #
#'

# load counties data
counties <- 
  readRDS('../../1_clean_data/sichuan_adm/counties_ref.rds')

# load lepto data
sp_lep <-
  readRDS(file = '../../1_clean_data/sichuan_database/sp_lep.rds')

# load vic data

raster_var <-
  readRDS(file = paste0('../../1_clean_data/sichuan_hydroDB/raster_vic_zhang_',
                        var,'.rds'))

#'
#' # Proceed to plot #
#'

vect_legend <-
  c("sm1"="\u03B8 (in mm)",  # \u03B8 is unicode for theta
    "roff"="Q (in mm/s)")
# see 
# https://stackoverflow.com/questions/27690729/greek-letters-symbols-and-line-breaks-inside-a-ggplot-legend-label
# for unicodes

vect_color_max <-
  c("sm1"="brown",
    "roff"="blue")

# format to dataframe
counties.df <- fortify(counties, region = "CNTY_CODE")

g_var <-
  rasterVis::gplot(raster_var[['X2003.09.01']]) +
  ggtitle(names(raster_var[['X2003.09.01']])) +
  geom_tile(aes(fill = value)) +
  geom_polygon(data=counties.df,
               aes(x = long, y = lat, group=group),
               color="gray90",fill=NA,size=0.3) +
  geom_point(data = as.data.frame(coordinates(sp_lep)),
             aes(x=x1,y=y1),alpha=0.1) +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude",
       fill=bquote(.(vect_legend[[var]])),
       alpha="") +
  # scale_fill_gradient2(low = 'white', high = 'blue') +
  scale_fill_gradient2(low = 'white',
                       high = muted(vect_color_max[[var]]),
                       mid = "white",midpoint = 0) +
  theme_void() +
  scalebar(counties.df, dist = 200,st.size = 5,st.bottom = F)  +
  north(counties.df,symbol = 3,scale = 0.15) 


g_var

# save as rds

pdf(file = paste0('../g_lep_',var,'.pdf'))
print(g_var)
dev.off()

