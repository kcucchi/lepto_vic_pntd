#'
#' In this file I create variables that I can call from everywhere
#'

env_China <- new.env()


env_China$vic_notations <- c("sm1"="theta_1","sm2"="theta_2")

saveRDS(object = env_China,file = 'env_China.rds')

