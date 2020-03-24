#'
#' In this script I analyze the cases per counties.
#' 
#'

library(sp)
library(maptools)
library(ggplot2)
library(ggsn)

library(dplyr)

library(scales)

library(RColorBrewer)

devtools::install_github("kcucchi/myUtils")


#' # Load data #
#' 
#' ## lepto data ##

# lepto spatial data
lep <- 
  readRDS(file = '../../../data_lepto/1_clean_data/sichuan_database/sp_lep.rds')
names(lep)

# lepto by county
df_lep_cnt <- readRDS('../../../data_lepto/2_formatted_data/df_cnt_lep.rds')
names(df_lep_cnt)

#' ## counties ##

counties <-
  readRDS('../../../../data/1_clean_data/sichuan_adm/counties_ref.rds')
counties@data$GBcode_cnt <- as.character(counties@data$GBcode_cnt)

plot(counties)
plot(lep,add=T)

#'
#' # Format data #
#'

gpclibPermit()

# ggplot plots dataframe
counties.df <-
  myUtils::sp2df(x_sp = counties)

# 
# counties@data$id <- rownames(counties@data)
# counties.df <- fortify(counties,region="id")
# counties.df <- dplyr::left_join(counties.df, counties@data, by="id")

# merge the cnt-lepto data with the county geometry
counties.df <- dplyr::left_join(x = counties.df,
                                y = df_lep_cnt,
                                by = c("GBcode_cnt"="CNTY_CODE"))
str(counties.df)

counties.df$count_farmers[counties.df$count_farmers==0] <- NA

#'
#' # plots #
#'

# g_cnt_lep <-
#   ggplot() +
#   geom_polygon(data = counties.df,
#                aes(x = long, y = lat, group=group,
#                    fill=count_farmers)) +  
#   geom_path(data = counties.df,aes(x = long, y = lat, group=group),
#             colour="black", lwd=0.001) + # county borders
#   # geom_point(data =  as.data.frame(coordinates(lep)),
#   #            aes(x=x1,y=y1,alpha="leptospirosis case"))+
#   coord_equal() +  
#   scale_fill_gradient2(low = "white", high = "red", # colors
#                        trans="log10",
#                        breaks = c(1,5,10,50,100,500)) +
#   labs(x = "Longitude", y = "Latitude",
#        fill = "counts\nby county") +
#   theme_void() + # remove axis ticks
#   scalebar(counties.df, dist = 200,st.size = 4,st.bottom = F)  +
#   north(counties.df,symbol = 3,scale = 0.15)
# 
# g_cnt_lep
# 
# # plotly::ggplotly(g_cnt_lep)
# 
# pdf('../g_cnt_lep.pdf',width=6,height = 5)
# print(g_cnt_lep)
# dev.off()

#'
#' # Save plot #
#'

# saveRDS(g_cnt_lep,file = 'g_cnt_lep.rds')

#'
#' # Also plot mean yearly incidence #
#'

df_cnt_all <- 
  readRDS(file = '../../../data_lepto/2_formatted_data/df_cnt_year_final.rds')
str(df_cnt_all)

df_cnt_year_lep_inc <-
  df_cnt_all %>%
  select(year,CNTY_CODE,pop_total,count) %>%
  mutate(year=as.numeric(year)) %>%
  subset(year > 2003) %>%
  mutate(inc=count/pop_total)

# calculate mean incidence over years
df_cnt_lep_inc <-
  df_cnt_year_lep_inc %>%
  group_by(CNTY_CODE) %>%
  summarise(mean_inc=mean(inc))

str(df_cnt_lep_inc)

range(df_cnt_lep_inc$mean_inc)

df_cnt_lep_inc$mean_inc_cat <-
  as.character(cut(x = df_cnt_lep_inc$mean_inc,
                   breaks = c(-Inf,0,0.1,0.2,0.4,0.6,0.8),
                   right = F))

unique(df_cnt_lep_inc$mean_inc_cat)

# isolate out zeros
df_cnt_lep_inc$mean_inc_cat[which(df_cnt_lep_inc$mean_inc == 0)] <- "0"
df_cnt_lep_inc$mean_inc_cat[which(df_cnt_lep_inc$mean_inc_cat == "[0,0.1)")] <- 
  "(0,0.1)"

unique(df_cnt_lep_inc$mean_inc_cat)

# define levels of factors
df_cnt_lep_inc$mean_inc_cat <- 
  factor(x = df_cnt_lep_inc$mean_inc_cat,
         levels = c("0","(0,0.1)","[0.1,0.2)",
                    "[0.2,0.4)","[0.4,0.6)","[0.6,0.8)"))

counties.df_inc <- dplyr::left_join(x = counties.df,
                                    y = df_cnt_lep_inc,
                                    by = c("GBcode_cnt"="CNTY_CODE"))

# g_cnt_lep_inc <-
#   ggplot() +
#   geom_polygon(data = counties.df_inc,
#                aes(x = long, y = lat, group=group,
#                    fill=mean_inc_cat)) +  
#   geom_path(data = counties.df,aes(x = long, y = lat, group=group),
#             colour="grey", lwd=0.001) + # county borders
#   coord_equal() +  
#   scale_fill_manual(values = c("white",
#                                brewer.pal(n = length(unique(df_cnt_lep_inc$mean_inc_cat)) - 1,
#                                           name = "YlOrRd"))) +
#   labs(x = "Longitude", y = "Latitude",
#        fill = "mean yearly incidence\nby county\n(per 100,000 population)") +
#   theme_void() + # remove axis ticks
#   scalebar(counties.df, dist = 200,st.size = 4,st.bottom = F)  +
#   north(counties.df,symbol = 3,scale = 0.15)
# 
# print(g_cnt_lep_inc)

#'
#' # add spatial clusters obtained by satscan analysis  #
#'

sp_north_nondf <- 
  readRDS(file = '../../satscan/sp_cluster_north.rds')

sp_south_nondf <- 
  readRDS(file = '../../satscan/sp_cluster_south.rds')

sp_north <-
  SpatialPolygonsDataFrame(sp_north_nondf, 
                           data.frame( ID=1:length(sp_north_nondf), 
                                       row.names = 1:length(sp_north_nondf),
                                       name="north"), 
                           match.ID = TRUE)

sp_south <-
  SpatialPolygonsDataFrame(sp_south_nondf, 
                           data.frame( ID=1:length(sp_south_nondf), 
                                       row.names = 1:length(sp_south_nondf),
                                       name="south"), 
                           match.ID = TRUE)

df_north <- myUtils::sp2df(x_sp = sp_north)
df_north$group <- 2
df_south <- myUtils::sp2df(x_sp = sp_south)

df_cluster <- rbind(df_north,df_south)

g_cnt_lep_inc_cluster <-
  ggplot() +
  geom_polygon(data = counties.df_inc,
               aes(x = long, y = lat, group=group,
                   fill=mean_inc_cat)) +  
  geom_path(data = counties.df,aes(x = long, y = lat, group=group),
            colour="grey70", lwd=0.001) + # county borders
  geom_polygon(data = df_cluster, 
               mapping = aes(x=long,y=lat,group=group,
                             col=name,linetype=name),
               fill=NA,size=1) +
  coord_equal() +  
  scale_fill_manual(values = c("white",
                               brewer.pal(n = length(unique(df_cnt_lep_inc$mean_inc_cat)) - 1,
                                          name = "YlOrRd"))) +
  scale_color_manual(values = c("north"=muted("red"),"south"="black")) +
  scale_linetype_manual(values = c("north"=5,"south"=1)) +
  labs(x = "Longitude", y = "Latitude",
       fill = "mean yearly incidence\n(per 100,000 population)",
       color = "cluster",
       linetype="cluster") +
  theme_void() + # remove axis ticks
  scalebar(counties.df, dist = 200,
           st.size = 2.5, st.dist = 0.05,
           st.bottom = F, transform=FALSE,
           dist_unit = "km")  #+
  # north(counties.df,symbol = 3,scale = 0.15) 

print(g_cnt_lep_inc_cluster)

pdf(file = '../g_cnt_lep_inc_cluster.pdf',width=6, height = 5)
print(g_cnt_lep_inc_cluster)
dev.off()

saveRDS(object = g_cnt_lep_inc_cluster,
        file = 'g_cnt_lep_inc_cluster.rds')



