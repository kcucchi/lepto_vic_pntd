# make simple figures showing the relationships between soil moisture, 
# rodents and leptospirosis incidence

library(ggplot2)
library(dplyr)
library(readxl)

#'
#' # Load data #
#'

# read annual soil moisture
sm_df <- readRDS(file = '../../data_lepto/2_formatted_data/df_year_vic.rds')
head(sm_df)

# read rodent data
rodents_df <- read_xlsx('../../data_lepto/0_raw_data/rodents/rodent_density.xlsx',
                        col_types = c('text', 'numeric'))
head(rodents_df)

# read leptospirosis incidence data
lepto_df <- readRDS(file = '../../data_lepto/2_formatted_data/df_year_lep.rds')
head(lepto_df)

# add population to dataframe
# get population from year by county data
lepto_cnt_df <- readRDS(file = '../../data_lepto/2_formatted_data/df_cnt_year_lep.rds')
head(lepto_cnt_df)
pop_cnt_df <- readRDS(file = '../../data_lepto/2_formatted_data/df_cnt_year_pop.rds')
head(pop_cnt_df)
lepto_df <- lepto_cnt_df %>%
  select(year, GBcode_cnt, count) %>%
  left_join(pop_cnt_df[, c('year', 'CNTY_CODE', 'pop_total')],
            by=c('year', "GBcode_cnt"="CNTY_CODE")) %>%
  filter(year >= 2004) %>%
  group_by(year) %>%
  summarise(count_lep = sum(count), pop=sum(pop_total)) %>%
  mutate(inc_per_tenthousand=count_lep/pop)
  
  

# merge all in one dataframe
all_df <- sm_df %>%
  select(year, vic_sm1) %>%
  left_join(rodents_df, by='year') %>%
  left_join(lepto_df[, c('year', 'inc_per_tenthousand')], by='year')
head(all_df)
all_df$year <- as.numeric(all_df$year)

#'
#' # Make plots #
#' 

g_sm_rod <-
  ggplot(data=all_df, 
         mapping = aes(x=vic_sm1, y=rodent_density)) +
  geom_text(aes(label=year)) +
  lims(x= c(26, 27.25)) +
  labs(x='theta (mm)', y='rodent density (%)') +
  theme_bw()

g_rod_lep <-
  ggplot(data=all_df,
         mapping = aes(x=rodent_density, y=inc_per_tenthousand)) +
  geom_text(aes(label=year)) +
  lims(x= c(4, 8)) +
  labs(x='rodent density (%)', y='leptospirosis incidence (per 10^5)') +
  theme_bw()

g_sm_rod_lep <-
  cowplot::plot_grid(g_sm_rod, g_rod_lep,
                     labels=c('A', 'B'))

print(g_sm_rod_lep)

pdf('g_sm_rod_lep.pdf', width=8, height=4)
print(g_sm_rod_lep)
dev.off()
