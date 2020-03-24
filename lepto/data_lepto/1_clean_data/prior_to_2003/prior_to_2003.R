
#'
#'---
#'author: Karina Cucchi
#'title: County-level lepto data prior to 2003
#'date: June 25th 2018
#'---
#'

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

path_raw <- '../../0_raw_data/prior_to_2003/1980-2003year.xlsx'

df_data <- read_xlsx(path = path_raw)

head(df_data)

df_data_pref <- 
  subset(x = df_data,
         subset = !is.na(city)) # do not consider counties

# do not consider total population
df_data_pref <- 
  df_data_pref[,!grepl(pattern = '^X',
                       x = names(df_data_pref))]

head(df_data_pref)

names(df_data_pref)

df_data_long <-
  df_data_pref %>%
  select(-area) %>%
  gather(key=year,value = count,-city) 

df_data_long$year <- as.numeric(df_data_long$year)
df_data_long$count <- as.numeric(df_data_long$count)

ggplot(df_data_long) +
  geom_line(mapping = aes(x=year,y=count,col=city)) +
  theme_bw() +
  scale_x_continuous(breaks = 1980:2003)

#'
#' # Sichuan only : format and save #
#'

df_data_sichuan <- 
  df_data %>%
  subset(subset = area == "sichuan province") %>%
  subset(select=!grepl(pattern = '^X',x = names(df_data))) %>%
  select(-city) %>%
  gather(key = year,value = count,-area)

df_data_sichuan$year <- as.numeric(df_data_sichuan$year)
df_data_sichuan$count <- as.numeric(df_data_sichuan$count)

#'
#' ## save data ##
#'

saveRDS(object = df_data_sichuan,file = 'df_lep_sichuan_1980-2003.rds')

#'
#' ## also plot ##
#' 

g_all <-
  ggplot(df_data_sichuan) +
  geom_bar(mapping = aes(x=year,y=count),stat = "identity") +
  theme_bw() +
  scale_x_continuous(breaks = seq(from=1980,to=2003,by = 5)) +
  geom_hline(mapping = aes(yintercept=800),col='grey') +
  geom_hline(mapping = aes(yintercept=0),col='black')

g_all

g_all + ylim(0,1e4)


