#' @title Calculation of onset and end of daily activity.
#' @description Using a vector of incubating individual presence in nest, 
#' \emph{incR.activity}
#' calculates onset of daily activity (first off-bout of a day),
#' and end of daily activity (last on-bout of a day). A column for dates, named "date"
#' is needed in the data argument.
#' @param data: data frame containing a time-series vector of 1s and 0s, where "1"
#' means "incubating individual inside nest" and "0" means "incubating individual 
#' outside the nests". This vector, 
#' under the name of "inc.vector" is provided by \code{\link{incR.scan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param vector.incubation: (character class) name of the vector containing information
#' for incubating individual presence/absence of nests. \code{\link{incR.scan}} produces
#' this vector named "inc.vector".
#' 
#' @return a data frame containing onset and end of activity times for each day in \emph{data}.
#' @author Pablo Capilla
#' @examples
#' To be included
#' @seealso \code{\link{incR.prep}} \code{\link{incR.scan}}
#' @export 
incR.activity <- function (data, vector.incubation) {
  # checking whether there is a date column
  if (base::is.null(data$date)) {
    stop("No column with name 'date' found")
  }
  
  # actual function
  ## day by day list
  df01 <- base::split (data, data$date)
  ## data frame to store data
  data.final <- base::data.frame (date=rep(NA, length=length(df01)),
                            onset=rep(NA, length=length(df01)),
                            offset=rep(NA, length=length(df01)))
  # loop to iteratively use each day for calculations
  for (k in 1:base::length(df01)) {
    df00 <- df01[[k]]                                      # k day
    data.final$date[k] <- base::as.character(base::unique(df00$date)) # kth day date
    
    # checking every point in "verctor.incubation" 
    for (s in 1:base::length(df00[[vector.incubation]])) {      # starting from 1st point of a day
      if (df00[[vector.incubation]][s] == 0) {            # when inc. ind. is first out
        data.final$onset[k] <- df00$dec.time[s]         # onset time
        break()                                         # break that day
      } else {
        next()                                          # if inc. ind. is in, next point
      }
    }
    # same loop as before to evaluate end of activity
    for (s in 1:base::length(base::rev(df00[[vector.incubation]]))) { # starting point from last one
      if (base::rev(df00[[vector.incubation]])[s] == 0) {       # when last time inc. ind. was out? 
        if (utils::head(base::rev(df00[[vector.incubation]]), 1)==0) { # does the time series for th day ends in 0. Then no end of activity is calculated.
          data.final$offset[k] <- NA                   
        } else {
          data.final$offset[k] <- base::rev(df00$dec.time)[s-1] # end point is the s-1 point - last onbout
        }
        break()                                           # one end point is got, finish loop
      } else {
        next()                                            # if inc. ind. is in, next point
      }
    }
  }
  return(data.final)
}