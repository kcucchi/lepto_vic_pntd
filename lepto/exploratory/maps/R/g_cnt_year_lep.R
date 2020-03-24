#' ---
#' title: "Plot maps of lepto counts by year-county"
#' author: "Karina Cucchi"
#' date: "September 20th, 2017"
#' ---

library(ggmap)
library(ggsn)

#'
#' # Load data #
#'

# load yearly-county level lepto dataset
df_lep <- readRDS('../../data/2_formatted_data/df_cnt_year_lep.rds')
str(df_lep)

df_lep <- subset(df_lep, as.numeric(year) >= 2004)
str(df_lep)

# load shapefile of sichuan counties

counties <- readRDS('../../data/1_clean_data/sichuan_adm/counties_ref.rds')
counties@data$CNTY_CODE <- as.character(counties@data$CNTY_CODE)

#'
#' # format data #
#'

names(counties)

# ggplot plots dataframe
counties@data$id <- rownames(counties@data)
counties.df <- fortify(counties,region="id")
counties.df <- dplyr::left_join(counties.df, counties@data, by="id")

# merge the lepto data with the county geometry
counties.df <- dplyr::left_join(x = counties.df,
                                y = df_lep,
                                by = "CNTY_CODE")
str(counties.df)

# replace zeros by NA
counties.df$count_farmers[which(counties.df$count_farmers==0)] <- NA

#'
#' # plot using ggmap #
#'

# define range for zoom by hand
pretty(counties.df$long)
pretty(counties.df$lat)
long_range <-c(0,range(counties.df$long)[2])
lat_range <- c(range(counties.df$lat)[1],
               3600000)

g_cnt_year_lep <-
  ggplot(data = counties.df,
         aes(x = long, y = lat, group=group,
             fill=count_farmers)) +
  geom_polygon() +
  geom_path(colour="black", lwd=0.001) + # county borders
  coord_equal() +
  facet_wrap(~ year,ncol = 3) +  # one plot per time slice
  scale_fill_gradient2(low = "white", high = "red", # colors
                       trans="log10",
                       breaks = c(1,5,10,50,100)) +
  labs(x = "Longitude", y = "Latitude",
       fill = "leptospirosis\ncounts\nby county") +
  coord_cartesian(xlim = long_range,
                  ylim = lat_range) + # zoom
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks


g_cnt_year_lep


pdf('../g_cnt_year_lep.pdf',width=9,height=9)
g_cnt_year_lep
dev.off()
