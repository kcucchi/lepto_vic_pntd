#'
#'---
#'title: Calculate correlation between environmental predictors
#'author: Karina Cucchi
#'date: Mar 6th 2018
#'---
#'


library(ggplot2)
library(lubridate)


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
#' # Remove NAs #
#'

names_vic <-
  # c("vic_Prcp","vic_roff","vic_sm1","vic_sm2","vic_sm3","vic_Tmin")
  c("vic_Prcp", "vic_roff", "vic_sm1", "vic_Tmin")
# for yearly data
idx_remove <-
  unique(which(is.na(df_year_vic[,names_vic]),arr.ind = T)[,'row'])
df_year_vic <- df_year_vic[setdiff(1:nrow(df_year_vic),idx_remove),]

# for weekly data
idx_remove <-
  unique(which(is.na(df_week_vic[,names_vic]),arr.ind = T)[,'row'])
df_week_vic <- df_week_vic[setdiff(1:nrow(df_week_vic),idx_remove),]


#'
#' # Calculate correlations #
#' 

# what is the correlation between variables?

cor_yearly <-
  as.matrix(cor(df_year_vic[,names_vic]))
cor_yearly

cor_weekly <-
  as.matrix(cor(df_week_vic[,names_vic]))
cor_weekly

# cool !

#'
#' # Try to plot this #
#' 

#'
#' first do for yearly data
#'

# reshape to long format
df_cor_yearly <- reshape2::melt(cor_yearly)
# keep only upper triangle of the matrix
df_cor_yearly <- 
  df_cor_yearly[which(as.numeric(df_cor_yearly$Var1) >= 
                        as.numeric(df_cor_yearly$Var2)),]

# plot 
g_vic_cor_year <-
  ggplot(data = df_cor_yearly) +
  geom_tile(mapping = aes(x=Var1,
                          y=ordered(Var2,rev(levels(df_cor_yearly$Var2))),
                          fill=value)) +
  geom_text(mapping = aes(x=Var1,y=Var2,label=round(value,2))) +
  # scale_fill_gradient2(low = "blue",mid = "white",high="red",midpoint = 0) +
  scale_x_discrete(labels=gsub(pattern = "vic_",
                               replacement = "",
                               x = levels(df_cor_yearly$Var1))) +
  scale_y_discrete(labels=gsub(pattern = "vic_",
                               replacement = "",
                               x = rev(levels(df_cor_yearly$Var2)))) +
  scale_fill_distiller(palette = "RdBu",limits = c(-1,1)) +
  labs(x="VIC variable",y="VIC variable",fill="correlation") +
  theme_bw() +
  theme(legend.position = "none")

g_vic_cor_year

pdf(file = '../g_vic_cor_year_2019-07-14.pdf',width = 3,height = 2)
print(g_vic_cor_year)
dev.off()

#'
#' same with weekly data
#'

# reshape to long format
df_cor_weekly <- reshape2::melt(cor_weekly)
# keep only upper triangle of the matrix
df_cor_weekly <- 
  df_cor_weekly[which(as.numeric(df_cor_weekly$Var1) >= 
                        as.numeric(df_cor_weekly$Var2)),]

# plot 
g_vic_cor_week <-
  ggplot(data = df_cor_weekly) +
  geom_tile(mapping = aes(x=Var1,
                          y=ordered(Var2,rev(levels(df_cor_weekly$Var2))),
                          fill=value)) +
  geom_text(mapping = aes(x=Var1,y=Var2,label=round(value,2))) +
  # scale_fill_gradient2(low = "blue",mid = "white",high="red",midpoint = 0) +
  scale_x_discrete(labels=gsub(pattern = "vic_",
                               replacement = "",
                               x = levels(df_cor_yearly$Var1))) +
  scale_y_discrete(labels=gsub(pattern = "vic_",
                               replacement = "",
                               x = rev(levels(df_cor_yearly$Var2)))) +
  scale_fill_distiller(palette = "RdBu",limits = c(-1,1)) +
  labs(x="VIC variable",y="VIC variable",fill="correlation") +
  theme_bw() +
  theme(legend.position = "none")

g_vic_cor_week

pdf(file = '../g_vic_cor_week_2019-07-14.pdf',width = 3,height = 2)
print(g_vic_cor_week)
dev.off()
