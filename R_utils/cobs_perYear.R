#' 
#' Generate B-splines for yearly outbreaks
#' 
#' @param df_week_year dataframe containing year and week number
#'  (in y and time fields respectively) 
#' @param dfPerYear number of basis splines to generate per year
#' 
#' @return a matrix of year-specific basis splines starting and ending at zero
#'

cobs_perYear <- function(df_week_year,dfPerYear){
  
  # calculate number of years
  unique_year <- unique(df_week_year$y)
  nbYear <- length(unique_year)
  
  # start dataframe for bases
  # start by time field (to be deleted before returned by the function)
  mkbasis <- data.frame(time=df_week_year$time)
  
  # split df_week_year by year 
  df_byYear <- split(df_week_year, f = df_week_year$y)
  
  # loop over years
  for(i in 1:length(df_byYear)){
    
    # get week numbers for that year
    time_i <- sort(unique(df_byYear[[i]]$time))
    # generate basis splines for these weeks (the last one doesn't stop at 0)
    mkbasis_i <- splines::bs(x = time_i,df = dfPerYear+1)
    # get rid of last one (not finishing at zero)
    mkbasis_i <- mkbasis_i[,setdiff(colnames(mkbasis_i),as.character(dfPerYear+1))]
    # rename columns
    colnames(mkbasis_i) <- paste0(unique_year[i],'_',colnames(mkbasis_i))
    # join the time value
    mkbasis_i <- data.frame(cbind(mkbasis_i,time=time_i))
    
    # join to global mkbasis variable
    mkbasis <- dplyr::left_join(mkbasis,mkbasis_i,by="time")
    
  }
  
  # fill in zeros
  mkbasis[is.na(mkbasis)] <- 0
  
  # remove time column
  mkbasis <- mkbasis[,setdiff(names(mkbasis),"time")]
  
  # change format to matrix
  mkbasis <- data.matrix(mkbasis)
  
  # return final result
  return(mkbasis)
  
}