#' ---
#' title: "Formats population data at the county level"
#' author: "Karina Cucchi"
#' date: "September 13th, 2017"
#' ---
#' 
#' In this script I load and format population data 
#' for further exploration at county-level.
#' 


library(plyr);library(dplyr)


#'
#' # Load data #
#' 

# load population dataset

df_pop <- readRDS('../../1_clean_data/sichuan_adm/sichuancountypop/sichuanCountyPop.rds')

# load reference county-time shapefiles
df_cnt_year <- readRDS(file = '../df_cnt_year_ref.rds')
df_cnt_week <- readRDS(file = '../df_cnt_week_ref.rds')

#'
#' # Format population dataframe #
#'

# reshape to long format
df_pop_long <- reshape2::melt(
  df_pop[,c('CNTY_CODE',
            names(df_pop)[grep(pattern = '^X',names(df_pop))])],
  id="CNTY_CODE",stringAsFactor = F)
colnames(df_pop_long) <- c("CNTY_CODE","variable","pop")
df_pop_long$variable <- as.character.factor(df_pop_long$variable)
df_pop_long$CNTY_CODE <- as.character(df_pop_long$CNTY_CODE)
str(df_pop_long)

# add year of population estimate
df_pop_long$year <- substr(df_pop_long$variable,start = 2,stop = 5)
# add whether total population or farmers only
df_pop_long$type <- substr(df_pop_long$variable,start = 6,stop = 11)
# get rid of unnecessary field
df_pop_long <- df_pop_long[,setdiff(names(df_pop_long),"variable")]

str(df_pop_long)

# cast pop and agrpop on 2 columns
df_pop_long <- reshape2::dcast(data = df_pop_long,
                               formula = CNTY_CODE + year ~ type,
                               value.var="pop")
names(df_pop_long) <- c("CNTY_CODE","year","pop_agr","pop_total")

str(df_pop_long)

#'
#' # Join to reference county-time dataframes #
#' 

#'
#' ## first do county-year ##
#'

#'
#' ### join to reference dataframe ###
#'

df_cnt_year_pop <- dplyr::left_join(x = df_cnt_year,
                                    y = df_pop_long,
                                    by = c("CNTY_CODE","year"))

str(df_cnt_year_pop)

# do linear interpolation by county for missing year values

df_cnt_year_pop$year <- as.numeric(df_cnt_year_pop$year)

vect_cnty <- unique(df_cnt_year_pop$CNTY_CODE)
vect_year <- unique(df_cnt_year_pop$year)

my.func <- function(df_cnt_year_pop){
  estimate_agr <- 
    approx(x = df_cnt_year_pop$year,
           y = df_cnt_year_pop$pop_agr,
           xout = vect_year,
           method = "linear", # linear interpolation
           rule = 2)          # values for years < 2005 are = values at year 2005
  estimate_total <- 
    approx(x = df_cnt_year_pop$year,
           y = df_cnt_year_pop$pop_total,
           xout = vect_year,
           method = "linear", # linear interpolation
           rule = 2)          # values for years < 2005 are = values at year 2005
  
  return(data.frame(year=vect_year, 
                    CNTY_CODE=unique(df_cnt_year_pop$CNTY_CODE),
                    pop_agr=estimate_agr$y,
                    pop_total=estimate_total$y,
                    stringsAsFactors = F))
}

df_cnt_year_pop <- plyr::ddply(df_cnt_year_pop, .(CNTY_CODE),  my.func)

# transform year back to character
df_cnt_year_pop$year <- as.character(df_cnt_year_pop$year)

str(df_cnt_year_pop)

#'
#' ### Save variable ###
#'

saveRDS(object = df_cnt_year_pop,
        file = '../df_cnt_year_pop.rds')

#'
#' ## Now do county-week ##
#'

#'
#' ### Add to reference dataframe ###
#'

# add year field to reference county-week dataframe
df_cnt_week$year <- format(x = as.Date(df_cnt_week$sundays), "%Y")
df_cnt_week$year[which(df_cnt_week$year=="2002")] <- "2003"

# join population values corresponding to that year
df_cnt_week_pop <- dplyr::left_join(x = df_cnt_week,
                                    y = df_cnt_year_pop,
                                    by=c("CNTY_CODE","year"))

# get rid of year field
df_cnt_week_pop <- 
  df_cnt_week_pop[,setdiff(names(df_cnt_week_pop),"year")]

str(df_cnt_week_pop)

#'
#' ### Save variable ###
#'
saveRDS(object = df_cnt_week_pop,
        file = '../df_cnt_week_pop.rds')


