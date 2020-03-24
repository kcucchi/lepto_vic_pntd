#'
#'---
#'author: Karina Cucchi
#'title: plot lepto cases from 1980 to 2014
#'date: June 25th 2018
#'---
#'

library(ggplot2)
library(scales)

#'
#' # Read data #
#'

df_lep_after2004 <- 
  readRDS(file = '../../../data_lepto/2_formatted_data/df_year_lep.rds')


head(df_lep_after2004)

df_lep_before2003 <- 
  readRDS(file = '../../../data_lepto/1_clean_data/prior_to_2003/df_lep_sichuan_1980-2003.rds')

head(df_lep_before2003)

#'
#' # Join in same dataframe #
#'

df_lep <-
  rbind(df_lep_before2003[,c('year','count')],
        df_lep_after2004[,c('year','count')])

#'
#' # Now plot #
#'

df_lep$NIDRS <- df_lep$year >= 2004
df_lep$year <- as.numeric(df_lep$year)

g_count_1980_2003 <-
  ggplot(df_lep) +
  geom_bar(mapping = aes(x=year,y=count,fill=NIDRS),
           stat="identity") +
  scale_x_continuous(breaks = seq(from=1980,to=2013,by=5)) +
  ylab("Sichuan counts") +
  scale_fill_manual(values = c("FALSE"="grey","TRUE"="brown2")) +
  theme_bw()

g_count_1980_2003

g_count_1980_2003 + xlim(1995,2013)

pdf(file = "../g_count_1980_2003.pdf",width = 4,height = 4)
cowplot::plot_grid(g_count_1980_2003,
                   g_count_1980_2003 + xlim(1995,2013),
                   ncol=1,
                   align="v")
dev.off()

g_count_1990_2014 <-
  ggplot() +
  geom_bar(data = df_lep,
           mapping = aes(x=year,y=count,fill=NIDRS),
           stat="identity") +
  geom_vline(mapping = aes(xintercept = 2003.5),
             linetype="dashed") +
  scale_x_continuous(breaks = seq(from=1980,to=2013,by=5)) +
  scale_y_continuous(name = "Leptospirosis cases", label = comma) +
  scale_fill_manual(values = c("FALSE"="grey","TRUE"="brown2")) +
  xlim(1989,2015) +
  theme_bw() +
  theme(legend.position="none")

pdf(file = "../g_count_1990_2014.pdf",width = 4,height = 2.5)
print(g_count_1990_2014)
dev.off()


