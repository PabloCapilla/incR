#' @title Calculation of the percentage of daily time spent in incubation.
#' @description This function calculates % of day time spent incubating
#' based on the "inc.vector" produced by \code{\link{incR.prep}}.
#' Current versions do not discriminate day and night times.
#' @param data: data frame containing a time-series vector of 1 and 0, where "1"
#' means "females inside nests" and "0" means "female" off the nests. This vector, 
#' under the name of "inc.vector" is provided by \code{\link{incR.scan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param sampling.rate: time between two consecutive temperature recordings.
#' @param vector.presence: name of the column (vector) storing the
#' information about the presence/ausence of the incubating indiviual in the nest.
#' @return Daily percentage (in 0 to 1 scale) of time in nest. Return in a 
#' data frame with one day per raw.
#' @examples
#' coming soon
#' @seealso \code{\link{incR.prep}} \code{\link{incR.scan}} \code{\link{incR.activity}}

incR_constancy<- function (data, sampling.rate, vector.presence) {
  ##### CHECKING FOR COLUMN NAMES #####
  if (is.null (data$date)){
    stop("No column for 'date")
  }
  # split data set by day
  data.time <- split (data, data$date)
  # data frame to fill later
  day.latency <- data.frame (date=rep(NA, length(data.time)))
  
  # loop to assess each day
  for (d in 1:length(data.time)) {
    day.latency$date[d] <- as.character(unique(data.time[[d]]$date)) #date
    day.in <- sum(data.time[[d]][[vector.presence]])                 # number of point inc.ind was in
    seg.day <- length(data.time[[d]][[vector.presence]])             # total length of the inc.vector
    day.latency$perc.in[d] <- (day.in/seg.day)                       # ratio
  }
  return (day.latency)
}
