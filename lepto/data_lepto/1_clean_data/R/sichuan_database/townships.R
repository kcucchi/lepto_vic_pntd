#' ---
#' title: "Read and save township geometry + population"
#' author: "Karina Cucchi"
#' date: "December 21st, 2017"
#' ---
#' 
#' In this script I load and format township data.
#' 

library(rgdal)

#'
#' # Read raw township file #
#'

# this is the township file
townships <- 
  rgdal::readOGR(dsn = '../../../0_raw_data/sichuan_database/from_gdb',
                 layer = 'Townships')
#'
#' # clean #
#'

#'
#' ## modify projection ##
#'

# this contains the desired projection
source('../../../refs.R')
# modify projection
townships <- spTransform(x = townships,CRSobj = ref_CRS)

#'
#' ## clean associated data ##
#'

# keep only ID column

# define same name as in counties_ref object
names(readRDS('../../sichuan_adm/counties_ref.rds'))
townships@data <- 
  data.frame(TWN_CODE=as.character.factor(townships@data$Object_ID),
             stringsAsFactors = F)

#'
#' # Add population data #
#'

# this has missing townships but has population data
townships_2004 <- 
  rgdal::readOGR(dsn = '../../../0_raw_data/sichuan_database/twnshps_incidence',
                 layer = '2004towninc')

length(townships);length(townships_2004)

# modify projection
townships_2004 <- 
  spTransform(x = townships_2004,CRSobj = ref_CRS)

# join data
names(townships_2004)

str(townships@data)
str(townships_2004@data[,c('Object_ID','total')])

# transform from integer to character
townships_2004@data$Object_ID <- as.character(townships_2004@data$Object_ID)
# rename name of population column (called total) to pop
names(townships_2004)[which(names(townships_2004)=="total")] <- "pop"

townships@data <- 
  dplyr::left_join(x = townships@data,
                   y = townships_2004@data[,c('Object_ID','pop')],
                   by = c("TWN_CODE"="Object_ID"))




#'
#' # save in rds file #
#'

saveRDS(object = townships,file = '../../sichuan_adm/townships_ref.rds')

#'
#' # plot #
#'

library(ggplot2)
library(ggsn)

# ggplot plots dataframe
townships@data$id <- rownames(townships@data)
townships.df <- ggplot2::fortify(townships,region="id")
townships.df <- dplyr::left_join(townships.df, townships@data, by="id")

g_twn_lep_pop <-
  ggplot() +
  geom_polygon(data = townships.df,
               aes(x = long, y = lat, group=group,fill=pop),
               col="black") +
  # geom_point(data = as.data.frame(coordinates(sp_lep)),
  #            aes(x=x1,y=y1),alpha=0.1) +
  coord_equal() +
  scale_fill_gradient2(low = "white", high = "red",
                       breaks = pretty(townships$pop),
                       limits=c(0,max(townships$pop,na.rm = T)),
                       labels = scales::comma) +
  labs(x = "Longitude", y = "Latitude",
       fill = "pop per township") +
  theme_void() +
  ggsn::scalebar(townships.df, dist = 200,st.size = 4,st.bottom = F)  +
  north(townships.df,symbol = 3,scale = 0.15)

g_twn_lep_pop

#'
#' compare with original data source
#'

townships_2004@data$id <- 
  rownames(townships_2004@data)
townships_2004.df <- 
  ggplot2::fortify(townships_2004,region="id")
townships_2004.df <- 
  dplyr::left_join(townships_2004.df, townships_2004@data, by="id")

ggplot() +
  geom_polygon(data = townships_2004.df,
               aes(x = long, y = lat, group=group,fill=pop),
               col="black") +
  # geom_point(data = as.data.frame(coordinates(sp_lep)),
  #            aes(x=x1,y=y1),alpha=0.1) +
  coord_equal() +
  scale_fill_gradient2(low = "white", high = "red",
                       breaks = pretty(townships_2004$pop),
                       limits=c(0,max(townships_2004$pop,na.rm = T)),
                       labels = scales::comma) +
  labs(x = "Longitude", y = "Latitude",
       fill = "pop per township") +
  theme_void() +
  ggsn::scalebar(townships_2004.df, dist = 200,st.size = 4,st.bottom = F)  +
  north(townships_2004.df,symbol = 3,scale = 0.15)

#'
#' seems to match, so join has been well done
#'