#'
#'---
#'title: Cross-correlation between lep and environmental predictors
#'author: Karina Cucchi
#'date: Mar 6th 2018
#'---
#'

library(dplyr)

library(ggplot2)
library(cowplot)

#'
#' # Yearly first #
#'

#' Load yearly lepto cases

df_year_lep <-
  readRDS(file = '../../../data/2_formatted_data/df_year_lep.rds')

str(df_year_lep)

#' Load yearly vic cases

df_year_vic <-
  readRDS(file = '../../../data/2_formatted_data/df_year_vic.rds')

str(df_year_vic)

#'
#' join dataframes
#'

df_year <- 
  dplyr::left_join(x = df_year_lep,y = df_year_vic,by = "year")

str(df_year)

rm(df_year_lep,df_year_vic)

#'
#'  select only variables of interest
#'  

names_vic <- c("vic_Prcp","vic_sm1","vic_roff","vic_Tmin")

df_year <- 
  subset(x = df_year,select = c("year","count",names_vic))

df_year_long <- 
  reshape2::melt(data = df_year,id = c("year","count"))

#'
#' # make plots #
#'

#' 
#' ## plot yearly lepto time series ##
#' 

g_year_lep <- 
  ggplot(df_year, aes(year, count)) +
  geom_bar(stat = "identity") +
  ggtitle("") + 
  xlab("") +
  ylab("Leptospirosis incidence counts") +
  theme_bw()

g_year_lep

pdf(file = 'g_year_lep.pdf',width = 5,height = 2.5)
print(g_year_lep)
dev.off()

#'
#' ## plot yearly vic time series ##
#' 

l_year_vic <- list()

for(i in 1:length(names_vic)){
  
  l_year_vic[[i]] <- 
    ggplot(data = subset(x = df_year_long,
                         subset = variable == names_vic[i]),
           mapping =  aes(year, value)) +
    geom_bar(stat = "identity") +
    ggtitle("") + 
    xlab("") +
    ylab(names_vic[i]) +
    theme_bw()
  
}

#' plot in same figure

g_year_vic <-
  plot_grid(plotlist = l_year_vic,ncol = 1)

print(g_year_vic)

pdf(file = 'g_year_vic.pdf',width = 5,height = 10)
print(g_year_vic)
dev.off()


#' 
#' ## plot cross-correlation ##
#' 

# remove last 2 years because missing data
df_year <- subset(x = df_year,subset = !is.na(vic_Prcp))

#' calculate cross-correlations

# this will contain the results
df_ccf <- data.frame(lag=0:5)

for(i in 1:length(names_vic)){
  
  ccf_yearly <- 
    ccf(x = as.ts(df_year$count),
        y = as.ts(df_year[,names_vic[i]]),
        lag.max = 50,plot = F,na.action = na.pass)
  
  df_ccf <- 
    dplyr::left_join(x = df_ccf,
                     y = data.frame(lag=ccf_yearly$lag[,1,1],
                                    vic_var=ccf_yearly$acf[,1,1]),
                     by = 'lag')
  
  names(df_ccf)[which(names(df_ccf) == "vic_var")] <- names_vic[i]
  
}



# calculate significant correlation level according to 
# https://stats.stackexchange.com/questions/3115/cross-correlation-significance-in-r
sign_level <- 2/sqrt(nrow(df_year))

df_ccf_long <- reshape2::melt(data = df_ccf,id = "lag")

l_year_ccf <- list()

for(i in 1:length(names_vic)){
  
  l_year_ccf[[i]] <- 
    ggplot(data = subset(x = df_ccf_long,
                         subset = variable == names_vic[i]),
           mapping =  aes(lag, value)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = sign_level,color="blue",linetype=2) +
    geom_hline(yintercept = 0) +
    ylim(-0.3,1) +
    ggtitle("") + 
    xlab(paste0("lag between counts and ",
                names_vic[i]," (years)")) +
    ylab("cross-correlation") +
    theme_bw()
  
}

#' plot in same figure

g_year_ccf <-
  plot_grid(plotlist = l_year_ccf,ncol = 1)

print(g_year_ccf)

pdf(file = 'g_year_ccf.pdf',width = 4,height = 10)
print(g_year_ccf)
dev.off()

# g_yearly_cc <-
#   ggplot(df_ccf_yearly,aes(x=lag,y=ccf)) +
#   geom_hline(yintercept = sign_level,color="blue",linetype=2) +
#   geom_bar(stat = "identity") +
#   scale_x_continuous(limits = c(-1,6),breaks=seq(-5,5)) +
#   ggtitle("") + 
#   xlab("lag between incidence counts and rainfall (year)") +
#   ylab("cross-correlation") +
#   ggtitle("Cross-correlation between yearly rainfall and incidence counts") 
# 
# g_yearly_cc


#'
#' ## plot yearly scatterplot ##
#'

list_g_scatter_year <- list()
for(i_vic in 1:length(names_vic)){
  list_g_scatter_year[[i_vic]] <-
    ggplot(data = df_year) +
    geom_smooth(mapping = aes_string(x=names_vic[i_vic],
                                     y="count"),
                method = "lm",col="black") +
    geom_point(mapping = aes_string(x=names_vic[i_vic],
                                    y="count",
                                    fill="year"),
               col="black",pch=21) +
    scale_fill_brewer(palette = "PiYG") +
    theme_bw() +
    theme(legend.position="none")
}


g_year_scatter <-
  plot_grid(plotlist = list_g_scatter_year,ncol = 1)

print(g_year_scatter)

pdf(file = 'g_year_scatter.pdf',width = 3,height = 10)
print(g_year_scatter)
dev.off()

