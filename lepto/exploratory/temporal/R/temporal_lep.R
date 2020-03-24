#' ---
#' title: "Plot time series of lepto data at weekly/yearly resolution"
#' author: "Karina Cucchi"
#' date: "September 18th, 2016"
#' ---
#' 


library(ggplot2)
library(lubridate)

#'
#' # load data #
#'

df_year <- 
  readRDS('../../../data_lepto/2_formatted_data/df_year_lep.rds')
df_year

df_week <- 
  readRDS('../../../data_lepto/2_formatted_data/df_week_lep.rds')

# get rid of years previous to 2004
df_year <- subset(x = df_year,subset = as.numeric(year) >= 2004)
df_week <- subset(x = df_week,
                  subset = lubridate::year(as.Date(x = df_week$sundays)) >= 2004)

str(df_week)


#'
#' # Format data #
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
  ggplot(data = df_year, aes(x=year,y=count)) +
  geom_bar(stat = "identity") +
  ylab('yearly count') + xlab('') +
  theme_minimal()

g_year

#'
#' ## entire weekly ## 
#'  

g_week <-
  ggplot(data = df_week,aes(x=sundays_date,y=count)) +
  geom_bar(stat = "identity") +
  ylab('weekly count') + xlab('') +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  theme_minimal()


g_week

#'
#' ##  weekly zoom ## 
#'  

g_weekZoom <-
  ggplot(data = df_week,
         aes(x=sundays_yearly,y=count,
             group=factor(y),color=factor(y))) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", 
               date_labels =  "%b",
               limits = as.Date(c("2000-08-01","2000-11-01"))) +
  labs(x="Month",y="weekly count",color="Year") +
  scale_color_brewer(palette = "PiYG") +
  theme_minimal() +
  guides(color=guide_legend(ncol=2))

g_weekZoom

dev.off()
pdf('../temporal_lep.pdf',width = 6,height = 8)
cowplot::plot_grid(g_year,g_week,g_weekZoom,ncol = 1)
dev.off()


#'
#' # Save plots #
#'

save(list = c('g_year','g_week','g_weekZoom'),file = 'temporal_lep.RData')



#'
#' # Fit unique cubic spline per year #
#'


# this dataframe will contain the results
df_spline_param <-
  data.frame(year = levels(df_week$y))
df_spline_param$x_0 <- numeric(nrow(df_spline_param))
df_spline_param$A <- numeric(nrow(df_spline_param))
df_spline_param$sigma <- numeric(nrow(df_spline_param))

df_week$doy <- lubridate::yday(as.Date(df_week$sundays))

for(i in 1:nrow(df_spline_param)){
  
  # subset to timeseries in latitude band and year
  df_subset_i <- 
    subset(x = df_week,
           subset = y == df_spline_param[i,'year'])
  
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


#' calculate mean and sd of timing of peak

df_spline_param$x_0_date <- 
  as.Date('2000-01-01') + df_spline_param$x_0

mean(df_spline_param$x_0_date)
sd(df_spline_param$x_0_date)
range(df_spline_param$x_0_date)

ggplot() +
  geom_histogram(data = df_spline_param,
                 mapping = aes(x_0_date,fill=year))

#' calculate mean and sd of duration

mean(df_spline_param$sigma)
sd(df_spline_param$sigma)
range(df_spline_param$sigma)

ggplot() +
  geom_histogram(data = df_spline_param,
                 mapping = aes(sigma,fill=year))


#' calculate mean and sd of amplitude

mean(df_spline_param$A)
sd(df_spline_param$A)

ggplot() +
  geom_histogram(data = df_spline_param,
                 mapping = aes(A,fill=year)) +
  xlim(0,250)


#' plot pairs

library(GGally)

my_scatter <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_point() +
    geom_smooth()
}

g_pairs <-
  ggpairs(df_spline_param,
          columns = c('x_0_date','A','sigma'),
          lower = list(continuous=my_scatter)) +
  theme_bw()

print(g_pairs)
