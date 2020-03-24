#' ---
#' title: "Latitudinal gradient"
#' author: "Karina Cucchi"
#' date: "March 19th, 2018"
#' ---
#' 
#' In this paper I investigate the better description/visualization
#'  of the latitudinal gradient present in lepto weekly time series.
#'  

library(lubridate)
library(ggplot2)
library(RColorBrewer)

library(myUtils) # to fit cubic spline

library(dplyr)

library(sp)

#'
#' # Load data #
#'

#' from heatmaps outputs... not ideal but will do for now

df_week_lat <- 
  readRDS(file = '../heatmaps/R/df_week_lat.rds')

str(df_week_lat)

#' also load population data (for incidence)

# population data used in the rest of the paper (regressions) 
# is in lepto/data_lepto/2_formatted_data/R/df_cnt_time_pop
df_pop <-
  readRDS(file = '../../data_lepto/2_formatted_data/df_cnt_week_pop.rds')
str(df_pop)

range(df_pop$pop_total)

# population in 10,000
df_pop$pop_total <- 1e4 * df_pop$pop_total

#'
#'need county shapefile to group population by latitude band
#'

sp_cnt <- 
  readRDS(file = '../../../data/1_clean_data/sichuan_adm/counties_ref.rds')
head(sp_cnt@data)

df_cnt <- sp_cnt@data
sp_cnt_latlong <- spTransform(sp_cnt, CRS("+proj=longlat +datum=WGS84"))
df_cnt$lat <- coordinates(sp_cnt_latlong)[,2]

# define factors for latitude band category
df_cnt$latInterval <- 
  cut(x = df_cnt$lat,breaks = seq(from=26,to=32.5,by=0.5))

# join with population data
df_pop <- 
  dplyr::left_join(x = df_pop,
                   y = dplyr::select(df_cnt,'GBcode_cnt','latInterval'),
                   by = c("CNTY_CODE"="GBcode_cnt")) %>%
  select(-pop_agr)

str(df_pop)

# sum population by latitude interval (and by time)

df_pop_latInterval <-
  df_pop %>%
  group_by(sundays,latInterval) %>%
  summarise(pop_total=sum(pop_total))
str(df_pop_latInterval)

# modify latitude interval to latitude mid
df_pop_latInterval$num_lat <- as.numeric(df_pop_latInterval$latInterval)
df_pop_latInterval$lat_mid <- 25.75 + df_pop_latInterval$num_lat * 0.5

head(df_pop_latInterval)
head(df_pop_latInterval$lat_mid)

# santy check of population
df_pop_latInterval %>%
  subset(sundays == '2005-09-25') %>%
  ggplot() +
  geom_bar(mapping = aes(x=lat_mid,y = pop_total),stat='identity') +
  coord_flip()

# orders of magnitude are ok (about 10 million around chengdu area)

#'
#' join to count data 
#'

str(df_week_lat)

df_week_lat <- 
  dplyr::left_join(x = df_week_lat,
                   y = select(df_pop_latInterval,'sundays','pop_total','lat_mid'),
                   by = c('sundays','lat_mid'))

str(df_week_lat)

# calculate incidence per 100,000 (idem than Figure 2 in the paper)
df_week_lat$inc <- df_week_lat$count / df_week_lat$pop_total * 1e5
hist(df_week_lat$inc)


#'
#' # Easy manip #
#'

# map all cases in same week
df_week_lat$sundays_withinYear <- 
  as.Date(df_week_lat$sundays)
year(df_week_lat$sundays_withinYear) <- 2000

# add field for year
df_week_lat$year <- year(as.Date(df_week_lat$sundays))

# get rid of year 2002-2003
df_week_lat <- subset(x = df_week_lat,
                      subset = year >= 2004)
head(df_week_lat)

#'
#' # Visualise in same figure #
#'

df_week_lat$year <- 
  factor(df_week_lat$year)
df_week_lat$lat_mid <- 
  factor(df_week_lat$lat_mid,
         levels = sort(unique(df_week_lat$lat_mid),decreasing = T))

# first visualize counts
ggplot() +
  geom_line(data = df_week_lat,
            mapping = aes(x = sundays_withinYear, y = count, 
                          col=year,group = year)) +
  labs(col="year") +
  scale_color_brewer(palette = "PiYG",direction = 1) +
  facet_wrap( ~ lat_mid,ncol=1,strip.position = "left") +
  theme_bw()

# now visualize incidence
ggplot() +
  geom_line(data = df_week_lat,
            mapping = aes(x = sundays_withinYear, y = inc, 
                          col=year,group = year)) +
  labs(col="year") +
  scale_color_brewer(palette = "PiYG",direction = 1) +
  facet_wrap( ~ lat_mid,ncol=1,strip.position = "left") +
  theme_bw()

#'
#' # Fit wave by latitude - year #
#'

# I want to fit a cubic B-spline to the time series

#'
#' ## first try with example timeseries ##
#' 


df_subset <- 
  subset(x = df_week_lat,subset = year == 2004 & lat_mid == 29.75)

head(df_subset)

# define day in year
df_subset$doy <- lubridate::yday(as.Date(df_subset$sundays))

# fit the spline
param_fit_subset <- 
  myUtils::spline_optim(t = df_subset$doy,y = df_subset$count)
df_subset$count_fit <- 
  myUtils::N_4(x = df_subset$doy,
               x_0 = param_fit_subset[['x_0']],
               A = param_fit_subset[['A']],
               sigma = param_fit_subset[['sigma']])

ggplot(data = df_subset) +
  geom_line(mapping = aes(x = doy,y = count)) +
  geom_line(mapping = aes(x = doy,y = count_fit),col='red')

#' nice. Now do it for all time series

#'
#' ## fit all time series ##
#' 

# this dataframe will contain the results
df_spline_param <-
  data.frame(
    expand.grid(
      lat_mid = factor(levels(df_week_lat$lat_mid)),
      year = levels(df_week_lat$year)))
df_spline_param$x_0 <- numeric(nrow(df_spline_param))
df_spline_param$A <- numeric(nrow(df_spline_param))
df_spline_param$sigma <- numeric(nrow(df_spline_param))

df_week_lat$doy <- lubridate::yday(as.Date(df_week_lat$sundays))

for(i in 1:nrow(df_spline_param)){
  
  # subset to timeseries in latitude band and year
  df_subset_i <- 
    subset(x = df_week_lat,
           subset = year == df_spline_param[i,'year'] & 
             lat_mid == df_spline_param[i,'lat_mid'])
  
  # fit parameters
  param_fit_i <- 
    myUtils::spline_optim(t = df_subset_i$doy,
                          y = df_subset_i$count)
  
  # save fitted parameters in dataframe
  df_spline_param[i,'x_0'] <- param_fit_i[['x_0']]
  df_spline_param[i,'A'] <- param_fit_i[['A']]
  df_spline_param[i,'sigma'] <- param_fit_i[['sigma']]
  
  
}

df_spline_param$sigma[which(df_spline_param$sigma < 0)] <-
  -1 * df_spline_param$sigma[which(df_spline_param$sigma < 0)]

#'
#' ## also fit incidence ##
#'


# this dataframe will contain the results
df_spline_param_inc <-
  data.frame(
    expand.grid(
      lat_mid = factor(levels(df_week_lat$lat_mid)),
      year = levels(df_week_lat$year)))
df_spline_param_inc$x_0 <- numeric(nrow(df_spline_param_inc))
df_spline_param_inc$A <- numeric(nrow(df_spline_param_inc))
df_spline_param_inc$sigma <- numeric(nrow(df_spline_param_inc))

df_week_lat$doy <- lubridate::yday(as.Date(df_week_lat$sundays))

for(i in 1:nrow(df_spline_param_inc)){
  
  # subset to timeseries in latitude band and year
  df_subset_i <- 
    subset(x = df_week_lat,
           subset = year == df_spline_param_inc[i,'year'] & 
             lat_mid == df_spline_param_inc[i,'lat_mid'])
  
  # fit parameters
  param_fit_i <- 
    myUtils::spline_optim(t = df_subset_i$doy,
                          y = df_subset_i$inc)
  
  # save fitted parameters in dataframe
  df_spline_param_inc[i,'x_0'] <- param_fit_i[['x_0']]
  df_spline_param_inc[i,'A'] <- param_fit_i[['A']]
  df_spline_param_inc[i,'sigma'] <- param_fit_i[['sigma']]
  
}

df_spline_param_inc$sigma[which(df_spline_param_inc$sigma < 0)] <-
  -1 * df_spline_param_inc$sigma[which(df_spline_param_inc$sigma < 0)]


#' what is the mean duration of the epidemics?

mean(df_spline_param$sigma)
sd(df_spline_param$sigma)

#'
#' # Plot results #
#'

options(warn = -1) 

#' simple manipulations
df_spline_param$x_0_withinYear <- 
  as.Date("2000-01-01") + round(df_spline_param$x_0)
df_spline_param$lat_mid_num <- 
  as.numeric(as.character.factor(df_spline_param$lat_mid))

head(df_spline_param)

#' check distribution of amplitude A
ggplot(data = df_spline_param) +
  geom_histogram(mapping = aes(A))

# a lot of amplitudes are at 0...
# keep only positive values in figures

g_lat_gradient_byYear <-
  ggplot(data = subset(df_spline_param,
                       A > 0)) +
  geom_point(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  col=year,size = A),
    alpha = 0.5) +
  geom_smooth(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  col=year,weight = A),
    se=F) +
  scale_color_brewer(palette = "PiYG") +
  labs(x = "latitude",y = "mean timing of cases",
       size="amplitude\nof peak") +
  coord_flip() +
  theme_bw() +
  theme(legend.box = "horizontal")

print(g_lat_gradient_byYear)

pdf(file = 'g_lat_gradient_byYear.pdf',width = 7,height = 3.5)
print(g_lat_gradient_byYear)
dev.off()

#' it's something...

#' 
#' keep it simple : 
#' do one smoothed regression, instead of one by year
#' 

g_lat_gradient <-
  ggplot(data = subset(df_spline_param,
                       A > 0)) +
  geom_point(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  col=year,size = A),
    alpha=0.8) +
  geom_smooth(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  weight = A),
    col="black",span = 0.6) +
  scale_color_brewer(palette = "PiYG") +
  labs(x = "latitude",
       y = "mean timing of cases",
       size = "amplitude\nof peak") +
  coord_flip() +
  theme_bw() +
  theme(legend.box = "horizontal")

print(g_lat_gradient)

pdf(file = 'g_lat_gradient.pdf',width = 7,height = 3.5)
print(g_lat_gradient)
dev.off()

saveRDS(object = g_lat_gradient,file = 'g_lat_gradient.rds')


#'
#' now do the same for incidence
#'

df_spline_param_inc$x_0_withinYear <- 
  as.Date("2000-01-01") + round(df_spline_param_inc$x_0)
df_spline_param_inc$lat_mid_num <- 
  as.numeric(as.character.factor(df_spline_param_inc$lat_mid))

ggplot(data = subset(df_spline_param_inc,
                     A > 0)) +
  geom_point(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  col=year,size = A),
    alpha = 0.5) +
  geom_smooth(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  col=year,weight = A),
    se=F) +
  scale_color_brewer(palette = "PiYG") +
  labs(x = "latitude",y = "mean timing of cases",
       size="amplitude\nof peak") +
  coord_flip() +
  theme_bw() +
  theme(legend.box = "horizontal")

g_lat_gradient_inc <-
  ggplot(data = subset(df_spline_param_inc,
                       A > 0)) +
  geom_point(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  col=year,size = A),
    alpha=0.8) +
  geom_smooth(
    mapping = aes(x = lat_mid_num,y = x_0_withinYear,
                  weight = A),
    col="black",span = 0.6) +
  # scale_color_brewer(palette = "PiYG") +
  # scale_color_manual(values = brewer.pal(n = 12,name = "Paired")[c(1:10,12)]) +
  # scale_color_brewer(palette = "GnBu") +
  # scale_color_manual(values = brewer.pal(n = 12,name = "GnBu")[c(1:10,12)]) +
  scale_color_manual(values = colorRampPalette(brewer.pal(n = 4,name = "BuPu"))(15)[5:15]) +
  labs(x = "latitude",
       y = "mean peak timing",
       size = "peak\nweekly\nincidence\n(per 100,000\npopulation)") +
  coord_flip() +
  theme_bw() +
  theme(legend.box = "horizontal")

g_lat_gradient_inc

pdf(file = 'g_lat_gradient_inc.pdf',width = 7,height = 3.5)
print(g_lat_gradient_inc)
dev.off()

saveRDS(object = g_lat_gradient_inc,file = 'g_lat_gradient_inc.rds')


#'
#' ## check relationship between amplitude and duration ##
#'

ggplot(data = df_spline_param) +
  geom_point(mapping = aes(x = A,y = sigma)) +
  geom_smooth(mapping = aes(x = A,y = sigma))

#'
#' duration and amplitude are slightly positively correlated 
#' for amplitudes bigger then ~5.
#' Lower than that, duration doesn't really make sense...
#'

#'
#' ## relationship between amplitude and latitude ##
#'

ggplot(data = subset(df_spline_param,
                     A > 0)) +
  geom_point(
    mapping = aes(x = lat_mid_num,y = A,
                  col=year),
    alpha=0.8) +
  # geom_smooth(
  #   mapping = aes(x = lat_mid_num,y = A,
  #                 weight = A),
  # col="black",span = 0.6) +
  scale_color_brewer(palette = "PiYG") +
  labs(x = "latitude",
       y = "amplitude of peak") +
  coord_flip() +
  theme_bw() +
  theme(legend.box = "horizontal")

#'
#' ## relationship between duration and latitude ##
#'

ggplot(data = subset(df_spline_param,
                     A > 0)) +
  geom_point(
    mapping = aes(x = lat_mid_num,y = sigma,
                  col=year,size = A),
    alpha=0.8) +
  geom_smooth(
    mapping = aes(x = lat_mid_num,y = sigma,
                  weight = A),
    col="black",span = 0.6) +
  scale_color_brewer(palette = "PiYG") +
  labs(x = "latitude",
       y = "mean duration (in days)",
       size = "amplitude\nof peak") +
  coord_flip() +
  theme_bw() +
  theme(legend.box = "horizontal")






