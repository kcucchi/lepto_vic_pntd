#' ---
#' title: "Plots for paper"
#' author: "Karina Cucchi"
#' date: "September 22th, 2017"
#' ---
#' 

library(ggplot2)
library(gridExtra)

library(ggsn)

#'
#' # Load data #
#'

load('../temporal/R/temporal_lep_inc.RData')

g_cnt_lep <- readRDS('../maps/R/g_cnt_lep_inc_cluster.rds')


#'
#' # Plot all on same #
#'

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

pdf('paper_descriptive_1.pdf',
    width = 19 * cm_to_in, height = 22 * cm_to_in)
gridExtra::grid.arrange(g_cnt_lep +
                          theme(legend.position = "bottom",
                                legend.direction='vertical') + 
                          ggtitle("A") +
                          theme(text = element_text(size=10),
                                legend.text = element_text(size = 10)),
                        g_year + ggtitle("B") +
                          theme(text = element_text(size=10),
                                legend.text = element_text(size = 10),
                                axis.text = element_text(size = 10)),
                        g_year_cluster + ggtitle("C") + 
                          theme(legend.position = "top",
                                text = element_text(size=10),
                                legend.text = element_text(size = 10),
                                axis.text = element_text(size = 10)),
                        g_week + ggtitle("D") +
                          scale_x_date(date_breaks = "2 year",
                                       date_labels = "%Y") +
                          theme(text = element_text(size=10),
                                legend.text = element_text(size = 8),
                                axis.text = element_text(size = 10)),
                        g_weekZoom + ggtitle("E") +
                          scale_x_date(date_breaks = "1 month", 
                                       date_labels =  "%m",
                                       limits = as.Date(c("2000-08-01","2000-11-01"))) +
                          theme(text = element_text(size=10),
                                legend.text = element_text(size = 10),
                                axis.text = element_text(size = 10)), 
                        layout_matrix = matrix(c(1, 1, 1, 1, 2, 3, 4, 5),
                                               nrow = 4, byrow = F))
dev.off()



#'
#' # Plot spatio-temporal in same figure #
#'

# read clustering results
g_cluster_byYear <-
  readRDS(file = '../satscan/township_level/R/g_cluster_byYear.rds')

print(g_cluster_byYear)

# read latitudinal gradient results
# g_lat_gradient <-
#   readRDS(file = '../lat_gradient/g_lat_gradient.rds')
g_lat_gradient_inc <-
  readRDS(file = '../lat_gradient/g_lat_gradient_inc.rds')

print(g_lat_gradient_inc)

pdf('paper_descriptive_2.pdf',
    width = 19 * cm_to_in, height = 10 * cm_to_in)
gridExtra::grid.arrange(
  g_cluster_byYear + theme(legend.position = "none") + ggtitle("A") +
    theme(text = element_text(size=10),
          legend.text = element_text(size = 10)),
  g_lat_gradient_inc  + ggtitle("B") +
    labs(x = "latitude",
         y = "mean peak timing",
         size = "peak\nweekly\nincidence\n(per\n100,000\npopulation)") +
    scale_y_date(breaks = as.Date(c("2000-09-01", "2000-09-15", "2000-10-01")), 
                 date_labels =  "%m/%d") +
    theme(text = element_text(size=10),
          legend.text = element_text(size = 10),
          axis.text = element_text(size = 10)),
  ncol=2)
dev.off()

