#' @title Calculation of the percentage of daily time spent in incubation.
#' @description This function calculates % of day time spent incubating
#' based on the "inc.vector" produced by \code{\link{incR.prep}}.
#' Current versions do not discriminate day and night times.
#' @param data: data frame containing a time-series vector of 1 and 0, where "1"
#' means "incubating individual inside nest" and "0" means "incubating individual
#'  outside nest". This vector, 
#' under the name of "inc.vector" is provided by \code{\link{incR.scan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param vector.incubation: name of the column (vector class) storing the
#' information about the presence/ausence of the incubating indiviual in the nest.
#' @return Daily percentage (in 0 to 1 scale) of time in nest. Return in a 
#' data frame with one day per raw.
#' @examples
#' coming soon
#' @seealso \code{\link{incR.prep}} \code{\link{incR.scan}} \code{\link{incR.activity}}
#' @export 

incR.constancy<- function (data, vector.incubation) {
  ##### CHECKING FOR COLUMN NAMES #####
  if (base::is.null (data$date)){
    stop("No column for 'date")
  }
  # split data set by day
  data.time <- base::split (data, data$date)
  # data frame to fill later
  day.latency <- base::data.frame (date=rep(NA, length(data.time)))
  
  # loop to assess each day
  for (d in 1:base::length(data.time)) {
    day.latency$date[d] <- base::as.character(base::unique(data.time[[d]]$date)) #date
    day.in <- base::sum(data.time[[d]][[vector.incubation]])                 # number of point inc.ind was in
    seg.day <- base::length(data.time[[d]][[vector.incubation]])             # total length of the inc.vector
    day.latency$perc.in[d] <- (day.in/seg.day)                       # ratio
  }
  return (day.latency)
}
