#'
#'---
#'title: check % rice cover and rice cover drop in Sichuan
#'author: Karina Cucchi
#'date: Aug 3rd 2018
#'---
#'

library(dplyr)
library(tidyr)

library(ggplot2)

library(plotly)

library(scales)

#'
#' # Load data #
#'

#'
#' land use
#'

# percent coverage by county
# computed by Charles

df_lu <-
  read.csv(file = paste0('../../../data/0_raw_data/sichuan_landUse/',
                         'academy_sciences/from_charles/merged_sichuan_rp.csv'),
           header = T,stringsAsFactors = F) %>%
  select(-X,-PYNAME) %>%
  mutate(GBcode_cnt=as.character(GBcode_cnt))
str(df_lu)

#'
#' counties
#'

sp_sichuan <- readRDS(file = '../../../data/1_clean_data/sichuan_adm/counties_ref.rds')

# cluster values
df_sichuan_cluster <- 
  readRDS(file = '../../exploratory/satscan/df_cluster_cnt.rds')

#'
#' # Calculate % difference #
#'

df_lu <-
  df_lu %>%
  mutate(diff_2005_2010 = ((PROP_2010-PROP_2005)/PROP_2005)*100,
         diff_2010_2015 = ((PROP_2015-PROP_2010)/PROP_2010)*100)

#'
#' # first visualisations #
#'


df_sichuan <- 
  myUtils::sp2df(x_sp = sp_sichuan)

df_sichuan <- dplyr::left_join(x = df_sichuan,
                               y = df_lu,
                               by = "GBcode_cnt")

str(df_sichuan)

ggplot(data = df_sichuan) +
  geom_polygon(mapping = aes(x = long,y = lat,group=group,
                             fill=PROP_2005),col='grey') +
  scale_fill_distiller(palette = "BuGn",direction=1) +
  coord_fixed() +
  theme_void()

ggplot(data = df_sichuan) +
  geom_polygon(mapping = aes(x = long,y = lat,group=group,
                             fill=PROP_2010),col='grey') +
  scale_fill_distiller(palette = "BuGn",direction=1) +
  coord_fixed() +
  theme_void()

ggplot(data = df_sichuan) +
  geom_polygon(mapping = aes(x = long,y = lat,group=group,
                             fill=PROP_2015),col='grey') +
  scale_fill_distiller(palette = "BuGn",direction=1) +
  coord_fixed() +
  theme_void()

ggplot(data = df_sichuan) +
  geom_polygon(mapping = aes(x = long,y = lat,group=group,
                             fill=diff_2005_2010),col='grey') +
  scale_fill_gradient2(midpoint = 0,
                       low = muted("red"),high = muted("green")) +
  coord_fixed() +
  theme_void()

ggplot(data = df_sichuan) +
  geom_polygon(mapping = aes(x = long,y = lat,group=group,
                             fill=diff_2010_2015),col='grey') +
  scale_fill_gradient2(midpoint = 0,
                       low = muted("red"),high = muted("green")) +
  coord_fixed() +
  theme_void()

#'
#' # Check histogram of values by cluster #
#'

df_all <- 
  dplyr::left_join(x = df_lu,
                   y = df_sichuan_cluster,
                   by = "GBcode_cnt") %>%
  gather(key='key',value = 'value',-GBcode_cnt,-CLUSTER)
head(df_all)

df_all[is.na(df_all)] <- 'none'

df_all$value <- as.numeric(df_all$value)

g_diff <-
ggplot(data = subset(df_all,key %in% c('diff_2005_2010','diff_2010_2015'))) +
  geom_histogram(mapping = aes(x=value,fill=CLUSTER),
               alpha=.5,position = "fill") +
  scale_fill_manual(values = c("1"="black","2"="red","none"="grey")) +
  scale_x_continuous(name = "% decrease in farmland",
                     limits = c(-5,2)) +
  facet_wrap( ~ key) +
  theme_bw()

print(g_diff)

ggplotly(g_diff)

g_dens_drop <-
  ggplot(data = subset(df_all,key %in% c("PROP_2005","PROP_2010","PROP_2015"))) +
  geom_density(mapping = aes(x=value,fill=CLUSTER),
               alpha=.5) +
  scale_fill_manual(values = c("1"="black","2"="red","none"="grey")) +
  scale_x_continuous(name = "% farmland cover",
                     limits = c(0,1)) +
  facet_wrap( ~ key) +
  theme_bw()

print(g_dens_drop)

ggplotly(g_dens_drop)

#'
#' # scatterplots of drop in lepto incidence vs drop in farmland area #
#'


# read lepto incidence by county-year
df_lep <- 
  readRDS(file = '../../data_lepto/2_formatted_data/df_cnt_year_lep.rds')
str(df_lep)

# calculate drop in 2005-2010 and 2010-2014
df_lep_drop <-
  df_lep %>%
  select(-count_farmers) %>%
  spread(key=year,value=count) %>%
  mutate(lep_drop_2005_2010=`2010`-`2005`,
         lep_drop_2010_2014=`2014`-`2010`)

str(df_lep_drop)

# join to table with all values
str(df_all)

df_all <-
  df_all %>%
  spread(key = key,value = value)
str(df_all)

df_all <- 
  dplyr::left_join(x = df_all,
                   y = select(df_lep_drop,
                              GBcode_cnt,lep_drop_2005_2010,lep_drop_2010_2014),
                   by = "GBcode_cnt")

str(df_all)

# make scatter plots
g_scatter_2005_2010 <-
  ggplot(df_all) +
  geom_point(mapping = aes(x=diff_2005_2010,y=lep_drop_2010_2014,
                           col=CLUSTER),alpha=0.5) +
  geom_smooth(mapping = aes(x=diff_2010_2015,y=lep_drop_2010_2014,
                            col=CLUSTER),alpha=0.5) +
  geom_smooth(mapping = aes(x=diff_2010_2015,y=lep_drop_2010_2014),alpha=0.5) +
  scale_color_manual(values = c("1"="black","2"="red","none"="grey")) +
  labs(x="% change in farmland with water cover",
       y="% change in number of leptospirosis cases") +
  theme_bw()

print(g_scatter_2005_2010)

ggplotly(g_scatter_2005_2010)

# make scatter plots
g_scatter_2010_2014 <-
  ggplot(df_all) +
  geom_point(mapping = aes(x=diff_2010_2015,y=lep_drop_2010_2014,
                           col=CLUSTER),alpha=0.5) +
  geom_smooth(mapping = aes(x=diff_2010_2015,y=lep_drop_2010_2014,
                            col=CLUSTER),alpha=0.5) +
  geom_smooth(mapping = aes(x=diff_2010_2015,y=lep_drop_2010_2014),alpha=0.5) +
  scale_color_manual(values = c("1"="black","2"="red","none"="grey")) +
  labs(x="% change in farmland with water cover",
       y="% change in number of leptospirosis cases") +
  theme_bw()

print(g_scatter_2010_2014)

ggplotly(g_scatter_2010_2014)


