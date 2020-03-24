#'
#'---
#'title: Pairwise scatterplots
#'author: Karina Cucchi
#'date: Mar 15th 2018
#'---
#'

library(ggplot2)
library(GGally)

#'
#' # load data #
#'

df_year_vic <- 
  readRDS('../../../data_lepto/2_formatted_data/df_cnt_year_vic.rds')
df_week_vic <-
  readRDS('../../../data_lepto/2_formatted_data/df_cnt_week_vic.rds')


#'
#' # Keep only pairs within study area #
#'

# read counts per county
df_cnt_lep <- 
  readRDS('../../../data_lepto/2_formatted_data/df_cnt_lep.rds')

#'
#' keep only counties with lepto cases
#'

df_year_vic <- 
  dplyr::left_join(x = df_year_vic,
                   y = df_cnt_lep,
                   by = c("GBcode_cnt" = "CNTY_CODE"))
df_year_vic <- 
  subset(df_year_vic,
         subset = cntWithLep)

df_week_vic <- 
  dplyr::left_join(x = df_week_vic,
                   y = df_cnt_lep,
                   by = c("GBcode_cnt" = "CNTY_CODE"))
df_week_vic <- 
  subset(df_week_vic,
         subset = cntWithLep)



#'
#' # plot pairs #
#'

options(warn = -1)

names_vic <- 
  # names(df_year_vic)[
  #   which(grepl(pattern = '^vic_',x = names(df_year_vic)))]
  c("vic_Prcp", "vic_roff", "vic_sm1", "vic_Tmin")

my_scatter <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_point(..., alpha = 0.1) +
    geom_smooth()
}

g_pairs_cnt_year_vic <-
  ggpairs(df_year_vic,
          columns = names_vic,
          lower = list(continuous=my_scatter)) +
  theme_bw()

g_pairs_cnt_year_vic



#'
#' same with bin_2d
#'

my_bin <- function(data, mapping, ..., low = "#132B43", high = "#56B1F7") {
  ggplot(data = data, mapping = mapping) +
    geom_bin2d(...) +
    scale_fill_gradient(low = "grey", high = "black") +
    geom_smooth()
}

g_pairs_cnt_year_vic <-
  ggpairs(df_year_vic,
          columns = names_vic,
          lower = list(continuous=my_bin)) +
  theme_bw()

g_pairs_cnt_year_vic

# I think I prefer this one

pdf(file = '../g_pairs_cnt_year_vic_2019-07-14.pdf',width = 5, height = 5)
print(g_pairs_cnt_year_vic)
dev.off()

#'
#' # Same for weeks now #
#'

g_pairs_cnt_week_vic <-
  ggpairs(df_week_vic,
          columns = names_vic,
          lower = list(continuous=my_bin)) +
  theme_bw()

g_pairs_cnt_week_vic

pdf(file = '../g_pairs_cnt_week_vic_2019-07-14.pdf',width = 5, height = 5)
print(g_pairs_cnt_week_vic)
dev.off()


