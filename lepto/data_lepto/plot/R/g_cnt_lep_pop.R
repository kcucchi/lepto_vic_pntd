#'
#' # Plot overlay of lepto and population varaiables#
#'

library(ggplot2)
library(ggsn)
library(sp)
library(scales)


#'
#' # Load data #
#'

# load counties data
counties <- 
  readRDS('../../1_clean_data/sichuan_adm/counties_ref.rds')

# load lepto data
sp_lep <-
  readRDS(file = '../../1_clean_data/sichuan_database/sp_lep.rds')

# load population data
df_pop <- 
  readRDS(file = '../../1_clean_data/sichuan_adm/sichuancountypop/sichuanCountyPop.rds')

# join pop data to counties sp
counties@data <- 
  dplyr::left_join(x = counties@data,
                   y = df_pop,
                   by = "CNTY_CODE")

head(counties@data)

#'
#' # Proceed to plot #
#'

# ggplot plots dataframe
counties@data$id <- rownames(counties@data)
counties.df <- ggplot2::fortify(counties,region="id")
counties.df <- dplyr::left_join(counties.df, counties@data, by="id")

g_cnt_lep_pop <-
  ggplot() +
  geom_polygon(data = counties.df,
               aes(x = long, y = lat, group=group,fill=X2014pop),
               col="black") +
  geom_point(data = as.data.frame(coordinates(sp_lep)),
             aes(x=x1,y=y1),alpha=0.1) +
  coord_equal() +
  scale_fill_gradient2(low = "white", high = "red",
                       breaks = pretty(counties$X2014pop),
                       limits=c(0,max(counties$X2014pop,na.rm = T)),
                       labels = scales::comma) +
  labs(x = "Longitude", y = "Latitude",
       fill = "total pop \nin 2014") +
  theme_void() +
  ggsn::scalebar(counties.df, dist = 200,st.size = 4,st.bottom = F)  +
  north(counties.df,symbol = 3,scale = 0.15)

g_cnt_lep_pop

pdf(file = paste0('../g_cnt_lep_pop.pdf'))
print(g_cnt_lep_pop)
dev.off()

g_cnt_lep_percAgr <-
  ggplot() +
  geom_polygon(data = counties.df,
               aes(x = long, y = lat, group=group,
                   fill=100*X2014agrpop/X2014pop),
               col="black") +
  geom_point(data = as.data.frame(coordinates(sp_lep)),
             aes(x=x1,y=y1),alpha=0.1) +
  coord_equal() +
  scale_fill_gradient2(low = "white", high = "darkgreen",
                       limits = c(0,100)) +
  labs(x = "Longitude", y = "Latitude",
       fill = "percentage of population\nin agriculture \nin 2014") +
  theme_void() +
  ggsn::scalebar(counties.df, dist = 200,st.size = 4,st.bottom = F)  +
  north(counties.df,symbol = 3,scale = 0.15)

g_cnt_lep_percAgr

pdf(file = paste0('../g_cnt_lep_percAgr.pdf'))
print(g_cnt_lep_percAgr)
dev.off()


