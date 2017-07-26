#' @title Calculation of the number of daily on- and off-bouts
#' @description Calculation of number and duration of incubation
#'  on- and off-bouts.
#' @param data data frame containing a time-series vector of 1's and 0's, where "1"
#' means "incubating individual inside nest" and "0" means "incubating individual
#' outside nest". This vector, 
#' under the name of "inc.vector", is provided by \code{\link{incRscan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param vector.incubation name of the column (vector class) storing the
#' information about the presence/absence of the incubating individual in the nest.
#' @param dec_time (character class) name of the column with decimal time.
#' @param temp (character class) name of the column with incubation temperatures.
#' @param sampling.rate time difference between two consecutive recording points.
#' Effectively, the rate at which data points were recorded 
#' (e.g. 1 data point per 50sec).
#' The time units of the returned object will depend on the units of this argument.
#' @return a 5-column data frame with one day of study per row.
#' Date, number on-bouts, number of off-bouts (number of on-bouts + 1 by definition) and 
#' mean time duration of on- and off-bouts are displayed in the 5 columns 
#' respectively. Mean times are shown in those time units you specify the argument
#' \emph{sampling.rate}.
#' @author Pablo Capilla-Lasheras
#' @examples
#' #' # loading example data
#' data(incR_procdata)
#' incRbouts (data=incR_procdata, 
#'            vector.incubation="incR_score", 
#'            dec_time="dec_time",
#'            temp="temperature",
#'            sampling.rate=240) # sampling rate in seconds.
#' @seealso \code{\link{incRprep}} \code{\link{incRscan}} \code{\link{incRactivity}}
#' \code{\link{incRconstancy}}
#' @export
incRbouts <- function (data, vector.incubation, dec_time, temp, sampling.rate) {
  ##### CHECKING FOR COLUMN NAMES #####
  if (base::is.null(data$date)){
    stop("No column for 'date")
  }
  # splitting df by day
  df01 <- base::split (data, data$date)
  
  # defining table to write results in
  data.days <- base::data.frame (date=base::rep(NA, length=base::length(df01)),
                                 number.on.bouts=base::rep(NA, length=base::length(df01)),
                                 number.off.bouts=base::rep(NA, length=base::length(df01)),
                                 mean.time.on.bout=base::rep(NA, length=base::length(df01)),
                                 mean.time.off.bout=base::rep(NA, length=base::length(df01)))
  
  # loop to fill table of results
  ## if only 2 days
  #if (base::length(df01)==2){
   # final.loop <- 2
  #} else {                              ## if more than 2 days...
   # final.loop <- base::length(df01)-1}
  
  # bout specific data table
  list.bouts <- base::as.list(NA)
  for (k in 1:length(df01)) {
    df00 <- df01[[k]] 
    # per day data
    rle_incR_score_values <- base::rle(df00[[vector.incubation]])$values
    rle_incR_score_length <- base::rle(df00[[vector.incubation]])$lengths
    # data frame to store results
    # defining table to write results in
    data.bouts <- base::data.frame (date=base::rep(NA, length=base::length(rle_incR_score_values)),
                                    type=base::rep(NA, length=base::length(rle_incR_score_values)),
                                    start_time=base::rep(NA, length=base::length(rle_incR_score_values)),
                                    duration=base::rep(NA, length=base::length(rle_incR_score_values)),
                                    start_temp=base::rep(NA, length=base::length(rle_incR_score_values)),
                                    final_temp=base::rep(NA, length=base::length(rle_incR_score_values)))
    data.bouts$date <- base::as.character(base::unique (df00$date))  # working date
    data.bouts$type <- base::ifelse(rle_incR_score_values == 1, "onbout", "offbout")
    
    # metrics per bout
    to_merge <- base::do.call(args = base::lapply(base::as.list(base::c(1:base::length(rle_incR_score_values))),
                 function(bout){
                   if(base::eval(bout) == 1){
                     start_time <- base::round(df00[[dec_time]][1], digits = 3)
                     duration <- rle_incR_score_length[bout] * sampling.rate
                     start_temp <- df00[[temp]][1]
                     final_temp <- df00[[temp]][base::sum(base::rle(df00[[vector.incubation]])$lengths[1:bout])]
                   } else {
                     start_time <- base::round(df00[[dec_time]][base::sum(base::rle(df00[[vector.incubation]])$lengths[1:(bout-1)])+1], 
                                               digits = 3)
                     duration <- rle_incR_score_length[bout] * sampling.rate
                     start_temp <- df00[[temp]][base::sum(base::rle(df00[[vector.incubation]])$lengths[1:(bout-1)])+1]
                     final_temp <- df00[[temp]][base::sum(base::rle(df00[[vector.incubation]])$lengths[1:bout])]
                   }
                   return(base::c(start_time, duration, start_temp, final_temp))
                 }),
                 what = "rbind")
                               
    
    data.bouts[,c(3:6)] <- to_merge
    list.bouts[[k]] <- data.bouts
  }
  
  # day specific data table
  # for each day
  for (k in 1:final.loop) {
    # selecting working day
    df00 <- df01[[k]] 
    
  
    data.days$date[k] <- base::as.character(base::unique (df00$date))  # working date
    
    # per day data
    rle_incR_score_values <- base::rle(df00[[vector.incubation]])$values
    rle_incR_score_length <- base::rle(df00[[vector.incubation]])$lengths
    
    data.days$number.off.bouts[k] <- base::length(rle_incR_score_values[rle_incR_score_values == 0])
    data.days$number.on.bouts[k] <- base::length(rle_incR_score_values[rle_incR_score_values == 1])
    
    data.days$mean.time.on.bout[k] <- base::mean(rle_incR_score_length[rle_incR_score_values == 1]) * sampling.rate
    data.days$mean.time.off.bout[k] <- base::mean(rle_incR_score_length[rle_incR_score_values == 0]) * sampling.rate
  }
  
  # output
  return(base::list(total_bouts = base::do.call(args = list.bouts, what = "rbind"),
              day_bouts = data.days))
}
  
  
  
  
  