#' @title Calculation of daily first incubation off-bout and last incubation on-bout
#' @description Using a vector of incubation scores, 
#' \emph{incRact}
#' calculates onset of activity (first off-bout in the morning),
#' and end of daily activity (last on-bout in the evening) per day. A column for dates, named "date"
#' is needed in the data argument.
#' @param data data frame containing a numeric vector of 1's and 0's (incubation scores), where "1"
#' means "incubating individual inside nest" and "0" means "incubating individual 
#' outside the nests". This vector, 
#' under the name of \code{incR_score} is provided by \code{\link{incRscan}} in the 
#' first object (\emph{incRscan_data}) of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param time_column (character class) name of the column containing times.
#' @param vector.incubation (character class) name of the vector containing 
#' incubation scores. \code{\link{incRscan}} produces
#' this vector named "incR_score".
#' @return a data frame containing fist off-bout and last on-bout per day in \emph{data}.
#' @author Pablo Capilla-Lasheras
#' @examples
#' #' # loading example data
#' data(incR_procdata)
#' incRact (data=incR_procdata, 
#'               time_column="time",
#'               vector.incubation="incR_score")
#' @seealso \code{\link{incRprep}} \code{\link{incRscan}}
#' @export 
incRact <- function (data, time_column, vector.incubation) {
  # checking whether there is a date column
  if (base::is.null(data$date)) {
    stop("No column with name 'date' found")
  }
  
  # actual function
  ## day by day list
  df01 <- base::split (data, data$date)
  ## data frame to store data
  data.final <- base::data.frame (date=rep(NA, length=length(df01)),
                            first_offbout=rep(NA, length=length(df01)),
                            last_onbout=rep(NA, length=length(df01)))
  # loop to iteratively use each day for calculations
  for (k in 1:base::length(df01)) {
    df00 <- df01[[k]]                                      # k day
    data.final$date[k] <- base::as.character(base::unique(df00$date)) # kth day date
    
    # checking every point in "verctor.incubation" 
    for (s in 1:base::length(df00[[vector.incubation]])) {      # starting from 1st point of a day
      if (df00[[vector.incubation]][s] == 0) {            # when inc. ind. is first out
        data.final$first_offbout[k] <- df00[[time_column]][s]         # onset time
        break()                                         # break that day
      } else {
        next()                                          # if inc. ind. is in, next point
      }
    }
    # same loop as before to evaluate end of activity
    for (s in 1:base::length(base::rev(df00[[vector.incubation]]))) { # starting point from last one
      if (base::rev(df00[[vector.incubation]])[s] == 0) {       # when last time inc. ind. was out? 
        if (utils::head(base::rev(df00[[vector.incubation]]), 1)==0) { # does the time series for th day ends in 0. Then no end of activity is calculated.
          data.final$last_onbout[k] <- NA                   
        } else {
          data.final$last_onbout[k] <- base::rev(df00[[time_column]])[s-1] # end point is the s-1 point - last onbout
        }
        break()                                           # one end point is got, finish loop
      } else {
        next()                                            # if inc. ind. is in, next point
      }
    }
  }
  return(data.final)
}