#' ---
#' title: "Analysis of fields in lepto database"
#' author: "Karina Cucchi"
#' date: "February  3rd, 2017"
#' ---
#' 
#' In this script I load and do simple analysis
#' of the fields in the lepto database 
#' (ie attribute table of All_Cases).
#' 

library(dplyr)
library(tidyr)

#'
#' # load data #
#'

# lepto_gdb <- readRDS(file = '../RData/lep_gdb_df.rds')
lepto_gdb <- 
  readRDS(file = '../../data_lepto/1_clean_data/sichuan_database/df_lep_gdb.rds')

str(lepto_gdb)

#'
#' # Check their age #
#'

# get rid of years old field
lepto_gdb$age_num <- 
  as.numeric(sub(pattern = " years old",
                 replacement = "",
                 x = lepto_gdb$age))
hist(lepto_gdb$age_num,xlab = "age (years)",main="age distribution")
# add age breaks in this histogram
age_breaks <- c(0,15,30,45,60,150) # define similar age categories as in Lau 2017 PNAS Ebola
abline(v=age_breaks,col='red',lty=2,lwd=2)

# calculate age categories
age_categories <- 
  c('0-14','15-29','30-44','45-59','60<=')
lepto_gdb$age_cat <- 
  age_categories[findInterval(x = lepto_gdb$age_num,vec = age_breaks)]

# breakdown age by year of illness

library(RColorBrewer)

counts <- table(lepto_gdb$age_cat,as.numeric(lepto_gdb$year))

# make Stacked Area Graph
library(ggplot2)

df_counts <- as.data.frame(counts)
colnames(df_counts) <- c('age','year','counts')
df_counts$year <- as.integer(levels(df_counts$year))[df_counts$year]

ggplot(df_counts, aes(x=year, y=counts, fill=age)) + geom_area()

for(i in 1:ncol(counts)){
  counts[,i] <- counts[,i] / sum(counts[,i])
}

df_counts <- as.data.frame(counts)
colnames(df_counts) <- c('ageAsPercentage','year','counts')
df_counts$year <- as.integer(levels(df_counts$year))[df_counts$year]

ggplot(df_counts, aes(x=year, y=counts, fill=ageAsPercentage)) + geom_area()

rm(df_counts,counts,i)

#'
#' check whether mortality increases with age
#'

# clean field for mortality
unique(lepto_gdb$date_death)
lepto_gdb$is_dead <-
  !(lepto_gdb$date_death %in% c(".", " "))

# calculate mortality rates
mortality_df <- lepto_gdb %>%
  group_by(age_cat) %>%
  summarise(n_death = sum(is_dead),
            n = n()) %>%
  mutate(death_freq = n_death / n) %>%
  drop_na()

g_mortality <-
  ggplot(mortality_df,
         aes(x = age_cat, y = 100 * death_freq)) +
    geom_bar(stat = "identity") +
    xlab("age category") +
    ylab("death rate (%)") +
    theme_bw()

print(g_mortality)

#'
#' ## heatmap with age and week ##
#'

# in the case that I want to check only for one year
# lepto_gdb <- subset(x = lepto_gdb,subset = (year == 2004))

counts <- table(age=lepto_gdb$age_cat,week=lepto_gdb$week)

# append zeros for weeks with no counts
# find complementary to 1-52
missing_weeks <- (1:52)[is.na(pmatch((1:52),as.numeric(colnames(counts))))]
append <- as.table(array(data = 0,dimnames = list(age_categories,missing_weeks),
                         dim = c(length(age_categories),length(missing_weeks))))
counts <- as.table(cbind(counts,append))
counts <- counts[,as.character(1:52)]

rm(missing_weeks,append)

df_counts <- as.data.frame(counts,stringsAsFactors = F)
names(df_counts) <- c("age","week","counts")
df_counts$week <- as.numeric(df_counts$week)

ggplot(df_counts, aes(week,age )) +
  geom_tile(aes(fill = counts), color = "white") +
  # geom_text(aes(label = round(counts, 1)),size=1.5,angle=90) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("age category") +
  xlab("week in year") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=12,face="bold")) +
  labs(fill = "counts")

#' normalize by number of cases in each group

for(i in 1:nrow(counts)){
  counts[i,] <- counts[i,] / sum(counts[i,])
}
df_counts <- as.data.frame(counts,stringsAsFactors = F)
names(df_counts) <- c("age","week","counts")
df_counts$week <- as.numeric(df_counts$week)

ggplot(df_counts, aes(week, age)) +
  geom_tile(aes(fill = counts), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  xlab("week in year") +
  ylab("age category") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=12,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "relative count\nby age category")

#' It seems that the first age category to get infected are the 05-14.
#' 


rm(counts,i)

# Do same plot but break down each age category in male vs female to check whether one sex gets infected before the other.

#'
#' # Check their gender #
#'

counts_gender <-
  as.data.frame(lepto_gdb %>%
                  group_by(Gender) %>%
                  summarise (n = n()) %>%
                  mutate(freq = n / sum(n)))

counts_gender$ratio <- c(100,
                         as.numeric(
                           100*subset(counts_gender,Gender=="male")['n']/
                             subset(counts_gender,Gender=="female")['n']))

# this is the ratio of expected male to female in Sichuan, 2000 population census
# taken from http://siteresources.worldbank.org/INTEAPREGTOPGENDER/Resources/Gender-Gaps-Figures&Facts.pdf
# Population Census Office under the State Council
mf_ratio <- 116.37

# breakdown by age category

counts <- table(lepto_gdb$Gender,lepto_gdb$age_cat)
# reshuffle to be in order
counts <- counts[,age_categories]
barplot(counts,beside = T,
        xlim=c(0, 3*ncol(counts) + 3),
        xlab="age",ylab="counts",
        # legend = rownames(counts),
        legend.text=T,
        args.legend=list(
          x=ncol(counts)-1,
          y=max(counts)
        ))

library(ggplot2)

counts_long <- reshape2::melt(counts)
names(counts_long) <- c("sex","age","counts")

g_sexByAge <- 
  ggplot(counts_long,aes(age,counts)) +
  geom_bar(stat = "identity", aes(fill = sex),position="stack") +
  ylab("Number of cases") +
  theme_bw()

g_sexByAge

#' Qu's remarks:
#' - careful because the men/women ratio is larger 1 in China with the unique child policy
#' - Qu wonders why there is a shortage of women in the 5-14 and 15-24 ranges

#' same graph taking into account the male/female ratio

# non-normalized graph shows p(sick & men)
# we want p(sick | men) = p(sick & men) / p(men)
counts['male',] <- counts['male',] / (mf_ratio / 100)

barplot(counts,beside = T,border = c('black','red'),
        xlim=c(0, 3*ncol(counts) + 3),
        xlab="age",ylab="counts",
        legend.text=T,
        args.legend=list(
          x=ncol(counts)-1,
          y=max(counts)
        ))

#' The red color indicates that the bars have been rescaled.

#'
#' ## break down age per year ##
#'

counts <- table(lepto_gdb$Gender,lepto_gdb$year)

counts_long <- reshape2::melt(counts)
names(counts_long) <- c("sex","year","counts")
counts_long$year <- as.factor(counts_long$year)

g_sexByYear <- 
  ggplot(counts_long,aes(year,counts)) +
  geom_bar(stat = "identity", aes(fill = sex),position="dodge") +
  ylab("Number of cases") +
  theme_bw()

g_sexByYear


#'
#' # Check their occupation #
#'

table_occupation <- table(lepto_gdb$occupation_trns)
# keep only if more than 10 occurences
table_occupation_main <- table_occupation[which(table_occupation>10)]
# add field "other"
table_occupation_main <- c(table_occupation_main,
                           other=sum(table_occupation) - sum(table_occupation_main))
par(mar=c(5,15,1,1))
barplot(table_occupation_main,horiz = T, las=2)

df_occupation_main <- data.frame(
  occupation_main=names(table_occupation_main),
  counts=as.numeric(table_occupation_main),
  perc=100*as.numeric(table_occupation_main)/
    sum(as.numeric(table_occupation_main)))

df_occupation_main

#'
#' # Stacked bar plot for occupation throughout years #
#'

# count how many for each year and occupation category
counts <- table(lepto_gdb$occupation_trns, lepto_gdb$year)

# to avoid too many categories only consider farmers, students and others
# first consider only farmers and students
counts_farmerStudent_temp <- rbind(Farmer=counts['Farmer',],
                                   Student=counts['Student',])
# now calculate everything except farmers and students
# and stack with farmers and students
counts_farmerStudent <- rbind(counts_farmerStudent_temp,
                              Other=colSums(counts) - 
                                colSums(counts_farmerStudent_temp))
rm(counts_farmerStudent_temp)

colors <- c("forestgreen",
            "gold",
            "#A7A7A7",
            "dodgerblue",
            "firebrick")

barplot(counts_farmerStudent, ylab="incidence counts",
        xlab="years", col=colors,
        legend = rownames(counts_farmerStudent)) 



counts_farmerStudent_long <- reshape2::melt(counts_farmerStudent)
names(counts_farmerStudent_long) <- c("occupation","year","counts")
counts_farmerStudent_long$year <- 
  as.factor(counts_farmerStudent_long$year)

g_occupationByYear <- 
  ggplot(counts_farmerStudent_long,aes(year,counts)) +
  geom_bar(stat = "identity",
           aes(fill = occupation),position="dodge") +
  ylab("Number of cases") +
  scale_x_discrete(breaks = seq(from = 2004,
                                  to = 2014,
                                  by = 2)) +
  scale_fill_manual(values=c("forestgreen",
                             "gold",
                             "#A7A7A7")) +
  theme_bw()

g_occupationByYear

# from the plos ntd letter:
# Figures must be less than 19.05cm in width, and less than 22.225cm in height
# pdf function takes values in inches
# let's make a figure that is almost at the limit

# from the plot ntd letter:
# We ask that all type be between 8-12 point (2.8 - 4.2 mm)
# It is still possible to read 5-6 point type (1.75 - 2.1mm)
# but should only be used sparingly in your figure
# point_to_mm = 0.35

cm_to_in = 0.393701

pdf("lepto_covariates_indb_barplots_article.pdf",
    width = 19 * cm_to_in, height= 10 * cm_to_in)
cowplot::plot_grid(g_occupationByYear +
                     theme(text = element_text(size=10),
                           legend.text = element_text(size = 10),
                           axis.text = element_text(size = 8)),
                   g_sexByAge + 
                     theme(text = element_text(size=10),
                           legend.text = element_text(size = 10),
                           axis.text = element_text(size = 8)),
                   # g_sexByYear,
                   g_mortality +
                     theme(text = element_text(size=10),
                           legend.text = element_text(size = 10),
                           axis.text = element_text(size = 8)),
                   ncol = 2,
                   align = 'v',
                   labels = c('A','B','C'),
                   axis = "l")
dev.off()

#'
#' Check occupation by sex
#'

lepto_gdb$occupation_trns_simple <- lepto_gdb$occupation_trns
# get indices of cases not Farmer or Student
idx_notStuFar <- which(!(lepto_gdb$occupation_trns %in% c("Farmer","Student")))
lepto_gdb$occupation_trns_simple[idx_notStuFar] <- "Other"


by.type <- dplyr::group_by(lepto_gdb,
                           occupation_trns_simple, Gender)
counts_occup_sex <- dplyr::summarise(by.type,
                                     count=n())
counts_occup_sex$percByOccup <- 
  c(c(871,1557)/(871+1557),
    c(26,102)/(26+102),
    c(63,320)/(63+320))
counts_occup_sex

g_occupationBySex <- 
  ggplot(counts_occup_sex,aes(occupation_trns_simple,count)) +
  geom_bar(stat = "identity", aes(fill = Gender),position="stack") +
  ylab("Number of cases") +
  # scale_fill_manual(values=c("forestgreen",
  #                            "gold",
  #                            "#A7A7A7")) +
  theme_bw()

g_occupationBySex

g_occupationBySexPerc <- 
  ggplot(counts_occup_sex,aes(occupation_trns_simple,percByOccup)) +
  geom_bar(stat = "identity", aes(fill = Gender),position="stack") +
  labs(x="Occupation",y="Number of cases") +
  theme_bw()

if(plotPdf) pdf("lepto_covariates_indb_genderByOccup.pdf",height=2,width=4)
g_occupationBySexPerc
if(plotPdf) dev.off()

#' Interesting, infected farmers are not overwhelmingly males
#' and infected students are not overwhelmingly females

#'
#' Check occupation by age
#'

by.type <- dplyr::group_by(lepto_gdb,
                           occupation_trns_simple, age_cat)
counts_occup_age <- dplyr::summarise(by.type,
                                     count=n())
counts_occup_age
counts_occup_age <- 
  counts_occup_age[which(!is.na(counts_occup_age$age_cat)),]



g_occupationByAge <- 
  ggplot(counts_occup_age,aes(age_cat,count)) +
  geom_bar(stat = "identity", aes(fill = occupation_trns_simple),
           position="stack") +
  labs(x="age category",y="Number of cases",fill="Occupation") +
  scale_fill_manual(values=c("forestgreen",
                             "#A7A7A7",
                             "gold")) +
  theme_bw() 

if(plotPdf) pdf("lepto_covariates_indb_occupByAge.pdf",width=4,height=2)
g_occupationByAge
if(plotPdf) dev.off()

#'
#' compare timing of peak for farmers vs students vs others
#' 

# add new field in lepto_gdb for farmer vs students vs others
lepto_gdb$occupation_trns_simple <- lepto_gdb$occupation_trns
lepto_gdb$occupation_trns_simple[
  which(!(lepto_gdb$occupation_trns_simple %in% c("Farmer","Student")))
  ] <- "Other"

counts <- table(occupation=lepto_gdb$occupation_trns_simple,week=lepto_gdb$week)

# append zeros for weeks with no counts
# find complementary to 1-52
missing_weeks <- (1:52)[is.na(pmatch((1:52),as.numeric(colnames(counts))))]
append <- as.table(array(data = 0,
                         dimnames = list(rownames(counts),missing_weeks),
                         dim = c(length(rownames(counts)),length(missing_weeks))))
counts <- as.table(cbind(counts,append))
# shuffle columns to have weeks in increasing order
counts <- counts[,as.character(1:52)]
# shuffle rows to have better order for plotting
counts <- counts[c("Farmer","Student"),]

rm(missing_weeks,append)

df_counts <- as.data.frame(counts,stringsAsFactors = F)
names(df_counts) <- c("occupation","week","counts")
df_counts$week <- as.numeric(df_counts$week)

ggplot(df_counts, aes(week,occupation )) +
  geom_tile(aes(fill = counts), color = "white") +
  # geom_text(aes(label = round(counts, 1)),size=1.5,angle=90) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("occupation") +
  xlab("week in year") +
  scale_y_discrete() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=12,face="bold")) +
  labs(fill = "counts")

#'
#' now also normalize by count in each category
#'

for(i in 1:nrow(counts)){
  counts[i,] <- counts[i,] / sum(counts[i,])
}

df_counts <- as.data.frame(counts,stringsAsFactors = F)
names(df_counts) <- c("occupation","week","counts")
df_counts$week <- as.numeric(df_counts$week)

ggplot(df_counts, aes(week,occupation )) +
  geom_tile(aes(fill = counts), color = "white") +
  # geom_text(aes(label = round(counts, 1)),size=1.5,angle=90) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  ylab("occupation") +
  xlab("week in year") +
  scale_y_discrete() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=12,face="bold")) +
  labs(fill = "counts")

#'
#' Is this graph related to the timing wrt to age category? (see earlier graph showing that age category 0-14 gets infected earlier than other age categories).
#' Is it related to the fact that students are younger than the rest of the population?

counts <- table(occupation=lepto_gdb$occupation_trns_simple,
                age_cat=lepto_gdb$age_cat)

counts

#'
#'  So yes early timing for 0-14 and for students are highly correlated !
#'

#'
#' # Check how many deaths caused by disease in dataset #
#'

nbDeath <- sum(!(lepto_gdb$date_death %in% c("."," ")))
pctDeath <- 100*nbDeath/nrow(lepto_gdb)
nbDeath;pctDeath

#'
#' # Check the field cases_location #
#'

par(mar=c(5,15,1,1))
barplot(table(lepto_gdb$cases_loca),las=2,horiz = T)


#'
#' # urban/rural #
#'

#'
#' We can get urban/rural from postal code
#' 
#'

# digits 1,2 : province
# digits 3,4 : city
# digits 5,6 : county
# for county : urban is 01-18 and 81-99, rural is 21-80

countyCode <- as.integer(substr(x = lepto_gdb$address_co,
                                start = 5,
                                stop = 6))
lepto_gdb$ruralUrban <- rep('urban',nrow(lepto_gdb))
lepto_gdb$ruralUrban[countyCode >= 21 &
                       countyCode < 80] <- 'rural'

counts <- table(lepto_gdb$ruralUrban,lepto_gdb$year)
barplot(counts,beside = T,
        legend.text=T,
        args.legend=list(
          x=3*ncol(counts)-1,
          y=max(counts)
        ))

for(i in 1:ncol(counts)){
  counts[,i] <- counts[,i]/sum(counts[,i])
}
barplot(100*counts,
        legend.text=T,
        ylab="proportion (%)",
        args.legend=list(
          x=ncol(counts)+2,
          y=max(100*counts)/3
        ))

#'
#' Check urban/rural vs occupation
#'

df_urbanVsOccupation <- 
  as.data.frame(table(lepto_gdb[,c('occupation_trns_simple','ruralUrban')]))

df_urbanVsOccupation

# percentage rural in farmers
100*2076/(2076+352)

# percentage rural in students
100*334/(334+49)

# percentage rural total
df_urban <- 
  as.data.frame(table(lepto_gdb[,c('ruralUrban')]))

100*2512/(2512+427)

#' Mmmm... I'm wondering if the urban vs rural classification works here... Farmers are not more rural than the rest of the population.
#' Let's compare with Sophie's results!
#' 

#'
#' # Are students and farmers located at similar locations ? #
#'

sichuan <- readRDS(file = '../RData/sichuan.rds')

# take lepto_gdb_1 (the one with most rows - but less finer scale)
# left_join with lepto_gdb which contains occupations, by OBJECTID

lep_gdb_1 <- readRDS(file = '../RData/lep_gdb_1.rds')

lep_gdb_1@data <- dplyr::left_join(x = lep_gdb_1@data,y=lepto_gdb,
                                   by="OBJECTID")

ggplot() +
  geom_polygon(fill="white",color='black',
               aes(x=long, y = lat, group = group),
               data = sichuan)  +
  coord_equal() +
  scale_x_continuous(name = 'longitude') +
  scale_y_continuous(name = 'latitude') +
  theme() +
  geom_point(aes(x=x1,y=y1,alpha=1/10,
                 colour = lep_gdb_1$occupation_trns_simple),
             data = as.data.frame(coordinates(lep_gdb_1))) +
  scale_colour_manual(name="occupation",  
                      values = c("Farmer"="forestgreen", 
                                 "Student"="gold",
                                 "Other"="grey"))

ggplot() +
  geom_polygon(fill="white",color='black',
               aes(x=long, y = lat, group = group),
               data = sichuan)  +
  coord_equal() +
  scale_x_continuous(name = 'longitude') +
  scale_y_continuous(name = 'latitude') +
  coord_cartesian(xlim = c(3e5,8e5),ylim = c(3.0e6,3.6e6)) +
  geom_point(aes(x=x1,y=y1,alpha=1/10,
                 colour = lep_gdb_1$occupation_trns_simple),
             data = as.data.frame(coordinates(lep_gdb_1))) +
  scale_colour_manual(name="occupation",  
                      values = c("Farmer"="forestgreen", 
                                 "Student"="gold",
                                 "Other"="grey"))

par(mfrow=c(1,2))

# plot maps of students
r <- raster::raster(sichuan,nrows=50, ncols=50)
idx_student <- which(lep_gdb_1$occupation_trns_simple=="Student")
nc <- 
  raster::rasterize(
    coordinates(lep_gdb_1[idx_student,]),
    r, fun='count', background=0)
plot(nc,xlim = c(3e5,8e5),ylim = c(3.0e6,3.6e6),
     col = RColorBrewer::brewer.pal(9,"YlOrBr"))
plot(sichuan, add=TRUE)

# plot maps of farmers
r <- raster::raster(sichuan,nrows=50, ncols=50)
idx_farmer <- which(lep_gdb_1$occupation_trns_simple=="Farmer")
nc <- raster::rasterize(
  coordinates(lep_gdb_1[idx_farmer,]),
  r, fun='count', background=0)
plot(nc,xlim = c(3e5,8e5),ylim = c(3.0e6,3.6e6),
     col = RColorBrewer::brewer.pal(9,"BuGn"))
plot(sichuan, add=TRUE)

#' zoom on southern cluster

par(mfrow=c(1,2))

# plot maps of students
r <- raster::raster(sichuan,nrows=50, ncols=50)
idx_student <- which(lep_gdb_1$occupation_trns_simple=="Student")
nc <- raster::rasterize(
  coordinates(lep_gdb_1[idx_student,]),
  r, fun='count', background=0)
plot(nc,xlim = c(3e5,7e5),ylim = c(3.0e6,3.4e6),
     col = RColorBrewer::brewer.pal(9,"YlOrBr"))
plot(sichuan, add=TRUE)

# plot maps of farmers
r <- raster::raster(sichuan,nrows=50, ncols=50)
idx_farmer <- which(lep_gdb_1$occupation_trns_simple=="Farmer")
nc <- 
  raster::rasterize(
    coordinates(lep_gdb_1[idx_farmer,]),
    r, fun='count', background=0)
plot(nc,xlim = c(3e5,7e5),ylim = c(3.0e6,3.4e6),
     col = RColorBrewer::brewer.pal(9,"BuGn"))
plot(sichuan, add=TRUE)

#' 
#' Remarks:
#' Almost no student in the northern cluster.
#' Similar students and farmer patterns overall.
#' 

# ABANBONNED : EASIER TO WORK FROM POSTAL CODES
# # make it possible to read chinese characters
# Sys.getlocale()
# Sys.setlocale(category = "LC_ALL", locale = "chs") #cht for traditional Chinese, etc.
# 
# # read file where addresses are saved with chinese characters
# lepto_gdb_chinese <- read.csv('../sichuan_database/all_cases_trns.csv',encoding="UTF-8",stringsAsFactors = F)
# # lepto_gdb_chinese <- read.csv('../sichuan_database/all_cases_addressOnly_2.txt',encoding="UTF-8",sep=';')
# 
# # adresses are in field address
# str(lepto_gdb_chinese$address)
# 
# # first do counties
# urbanCounty <- '区'
# ruralCounty <- '县'
# lepto_gdb_chinese$is_urbanCounty <- grepl(pattern = urbanCounty,x = lepto_gdb_chinese$address)
# lepto_gdb_chinese$is_ruralCounty <- grepl(pattern = ruralCounty,x = lepto_gdb_chinese$address)
# 
# # now do townships
# 
# urbanTwnshp <- '镇'
# ruralTwnshp <- '乡'
# lepto_gdb_chinese$is_urbanTwnshp <- grepl(pattern = urbanTwnshp,x = lepto_gdb_chinese$address)
# lepto_gdb_chinese$is_ruralTwnshp <- grepl(pattern = ruralTwnshp,x = lepto_gdb_chinese$address)
# 
# #' 
# #' ## sanity checks ##
# #'
# 
# ## weird there are counties that correspond to rural and urban locations
# sum(lepto_gdb_chinese$is_urbanCounty & lepto_gdb_chinese$is_ruralCounty)
# head(lepto_gdb_chinese$address[lepto_gdb_chinese$is_urbanCounty & 
#                                 lepto_gdb_chinese$is_ruralCounty])
# 
# ## same for townships
# sum(lepto_gdb_chinese$is_urbanTwnshp & lepto_gdb_chinese$is_ruralTwnshp)
# head(lepto_gdb_chinese$address[lepto_gdb_chinese$is_urbanTwnshp & 
#                                 lepto_gdb_chinese$is_ruralTwnshp])
# 
# # how many counties not classified
# sum(!lepto_gdb_chinese$is_urbanCounty & !lepto_gdb_chinese$is_ruralCounty)
# 
# # how many townships not classified
# sum(!lepto_gdb_chinese$is_urbanTwnshp & !lepto_gdb_chinese$is_ruralTwnshp)

