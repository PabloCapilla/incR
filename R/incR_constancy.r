#' @title Calculation of percentage of daily time spent in incubation.
#' @description not available
#' @param data: NA
#' @param sampling.rate: NA
#' @param vector.presence: NA
#' 
#' @return 
#' 
#' @examples
#' 
#' @seealso \code{\link{incR_prep}} \code{\link{incR_scan}} \code{\link{incR_activity}}



incR_constancy<- function (data, sampling.rate, vector.presence="female") {
  # split data set by day
  data.time <- split (data, data$date)
  # data frame to fill later
  day.latency <- data.frame (date=rep(NA, length(data.time)))
  
  # loop to assess each day
  for (d in 1:length(data.time)) {
    day.latency$date[d] <- as.character(unique(data.time[[d]]$date))
    day.in <- sum(data.time[[d]][[vector.presence]])
    seg.day <- length(data.time[[d]][[vector.presence]])
    
    day.latency$perc.in[d] <- (day.in/seg.day)
    
  }
  return (day.latency)
}
