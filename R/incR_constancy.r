#' @title Calculation of percentage of daily time spent in incubation.
#' @description This function calculates % of day time spent incubating
#' based on the "inc.vector" produced by \code{\link{incR_prep}}.
#' Current versions do not discriminate day and night times.
#' @param data: data frame containing a time-series vector of 1 and 0, where "1"
#' means "females inside nests" and "0" means "female" off the nests. This vector, 
#' under the name of "inc.vector" is provided by \code{\link{incR_scan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param sampling.rate: time between two consecutive temperature recordings.
#' @param vector.presence: name of the column (vector) storing the
#' information about the presence/ausence of the incubating indiviual in the nest.
#' 
#' @return Daily percentage (in 0 to 1 scale) of time in nest.
#' @examples
#' coming soon
#' @seealso \code{\link{incR_prep}} \code{\link{incR_scan}} \code{\link{incR_activity}}



incR_constancy<- function (data, sampling.rate, vector.presence) {
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
