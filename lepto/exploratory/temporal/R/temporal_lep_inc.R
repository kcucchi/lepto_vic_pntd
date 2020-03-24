#' ---
#' title: "Plot time series of lepto incidence at weekly/yearly resolution"
#' author: "Karina Cucchi"
#' date: "September 18th, 2016"
#' ---
#' 


library(ggplot2)
library(lubridate)

library(RColorBrewer)

library(dplyr)

library(scales)

#'
#' # load data #
#'

df_year_lep <- 
  readRDS('../../../data_lepto/2_formatted_data/df_year_lep.rds')
df_year_lep

df_week_lep <- 
  readRDS('../../../data_lepto/2_formatted_data/df_week_lep.rds')

# get rid of years previous to 2004
df_year_lep <- 
  subset(x = df_year_lep,subset = as.numeric(year) >= 2004)
str(df_year_lep)

df_week_lep <- 
  subset(x = df_week_lep,
         subset = lubridate::year(as.Date(x = df_week_lep$sundays)) >= 2004)
str(df_week_lep)

#'
#' ## population data ##
#'

# read population data
df_cnt_year_pop <-
  readRDS(file = '../../../data_lepto/2_formatted_data/df_cnt_year_pop.rds')
str(df_cnt_year_pop)

df_cnt_week_pop <-
  readRDS(file = '../../../data_lepto/2_formatted_data/df_cnt_week_pop.rds')
str(df_cnt_week_pop)


#'
#' # Format data #
#' 

#'
#' ## calculate incidence data ##
#'

#'
#' sum population data
#'

df_year_pop <-
  df_cnt_year_pop %>%
  group_by(year) %>%
  summarise(pop=sum(pop_total))

df_week_pop <-
  df_cnt_week_pop %>%
  group_by(sundays) %>%
  summarise(pop=sum(pop_total))

df_year <- 
  dplyr::left_join(x = df_year_lep,
                   y = df_year_pop,
                   by = "year") %>%
  mutate(inc=count/pop,
         year=as.numeric(year))

df_week <-
  dplyr::left_join(x = df_week_lep,
                   y = df_week_pop,
                   by = "sundays") %>%
  mutate(inc=count/pop)

#'
#' also calculate in southern/northern cluster at yearly level
#'

# get lepto counts by county-year
df_cnt_year_lep <- 
  readRDS(file = '../../../data_lepto/2_formatted_data/df_cnt_year_lep.rds')

# read clusters
df_cnt_clusters <-
  readRDS(file = '../../satscan/df_cluster_cnt.rds')

# link lepto, population and cluster data
df_all_clusters <-
  dplyr::left_join(x = select(df_cnt_year_pop,year, CNTY_CODE, pop_total),
                   y = select(df_cnt_year_lep,year,GBcode_cnt,count),
                   by = c("CNTY_CODE"="GBcode_cnt","year"))

df_all_clusters <-
  dplyr::left_join(x = df_all_clusters,
                   y = df_cnt_clusters,
                   by = c("CNTY_CODE"="GBcode_cnt"))

df_all_clusters_inc <-
  df_all_clusters %>%
  mutate(year=as.numeric(year)) %>%
  subset(year >= 2004) %>%
  subset(!is.na(CLUSTER)) %>%
  group_by(CLUSTER,year) %>%
  summarise(pop_total=sum(pop_total),
            count=sum(count)) %>%
  mutate(inc=count/pop_total)

df_all_clusters_inc$CLUSTER <- 
  gsub(pattern = "1",replacement = "south",x = as.character(df_all_clusters_inc$CLUSTER))


df_all_clusters_inc$CLUSTER <- 
  gsub(pattern = "2",replacement = "north",x = as.character(df_all_clusters_inc$CLUSTER))

#'
#' ## misc formatting ##
#' 

df_week$sundays_date <- as.Date(x = df_week$sundays)
df_week$y <- as.factor(lubridate::year(df_week$sundays_date))
# define new temporal field where all dates are within same year
df_week$sundays_yearly <- df_week$sundays_date
lubridate::year(df_week$sundays_yearly) <- 2000

str(df_week)


#'
#' # Make plot #
#'

#'
#' ## barplot by year ## 
#'  

g_year <- 
  ggplot() +
  geom_line(data = df_year, aes(x=year,y=inc)) +
  expand_limits(y = 0) +
  ylab('yearly incidence\n(per 100,000 population)') + xlab('') +
  theme_minimal()

g_year

g_year_cluster <- 
  ggplot() +
  geom_line(data = df_all_clusters_inc,
            mapping = aes(x=year,y=inc,
                          col=factor(CLUSTER),
                          linetype=factor(CLUSTER))) +
  expand_limits(y = 0) +
  scale_color_manual(name="cluster",
                     values = c("south"="black","north"=muted("red"))) +
  scale_linetype_manual(name="cluster",
                        values=c("south"=1,"north"=5)) +
  ylab('yearly incidence\n(per 100,000 population)') + xlab('') +
  theme_minimal() 

g_year_cluster



#'
#' ## entire weekly ## 
#'  

g_week <-
  ggplot(data = df_week,aes(x=sundays_date,y=inc)) +
  geom_bar(stat = "identity") +
  ylab('weekly incidence\n(per 100,000 population)') + xlab('') +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  theme_minimal()

g_week

#'
#' ##  weekly zoom ## 
#'  

g_weekZoom <-
  ggplot(data = df_week,
         aes(x=sundays_yearly,y=inc,
             group=factor(y),color=factor(y))) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b",
               limits = as.Date(c("2000-08-01","2000-11-01"))) +
  labs(x="Month",y="weekly incidence\n(per 100,000 population)",color="Year") +
  scale_color_manual(values = brewer.pal(n = 12,name = "Paired")[c(1:10,12)]) +
  theme_minimal() +
  guides(color=guide_legend(ncol=2)) 

g_weekZoom

dev.off()
pdf('../temporal_lep_inc.pdf',width = 6,height = 8)
print(cowplot::plot_grid(g_year,g_year_cluster + theme(legend.position="none"),
                         g_week,g_weekZoom,ncol = 1))
dev.off()


#'
#' # Save plots #
#'

save(list = c('g_year','g_year_cluster','g_week','g_weekZoom'),
     file = 'temporal_lep_inc.RData')


#'
#' # Additional check of incidence values #
#'

df_year$periods <- c(rep('2004-2005',2),
                     rep('2006-2010',5),
                     rep('2011-2014',4))
df_year %>%
  group_by(periods) %>%
  summarize(mean_inc=mean(inc))


