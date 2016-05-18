#' @title Calculation of the number of daily on- and off-bouts
#' @description Calculation of number and duration of incubation
#'  on- and off-bouts.
#' @param data data frame containing a time-series vector of 1 and 0, where "1"
#' means "incubating individual inside nest" and "0" means "incubating individual
#' outside nest". This vector, 
#' under the name of "inc.vector", is provided by \code{\link{incRscan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param vector.incubation name of the column (vector class) storing the
#' information about the presence/absence of the incubating indiviual in the nest.
#' @param sampling.rate time difference between two consecutive recording points.
#' Effectively, the rate at which data points were recorded 
#' (eg. 1 data point per 50sec).
#' The time units of the returned object will depend on the units of this argument.
#' @return a 5-column data frame with one day of study per row.
#' Date, number on-bouts, number of off-bouts (number of on-bouts + 1 by definition) and 
#' mean time duration of on- and off-bouts are displayed in the 5 columns 
#' respectively. Mean times are shown in those time units you specify the argument
#' \emph{sampling.rate}.
#' @author Pablo Capilla
#' @examples
#' #' # loading example data
#' data(incRincubationExample)
#' incRbouts (data=incRincubationExample, 
#'             vector.incubation="inc.vector", 
#'             sampling.rate=240) # sampling rate in seconds.
#' @seealso \code{\link{incRprep}} \code{\link{incRscan}} \code{\link{incRactivity}}
#' \code{\link{incRconstancy}}
#' @export
incRbouts <- function (data, vector.incubation, sampling.rate) {
  ##### CHECKING FOR COLUMN NAMES #####
  if (base::is.null (data$date)){
    stop("No column for 'date")
  }
  # splitting df by day
  df01 <- base::split (data, data$date)
  
  # defining table to write results in
  data.final <- base::data.frame (date=rep(NA, length=base::length(df01)),
                                  number.on.bouts=rep(NA, length=base::length(df01)),
                                  number.off.bouts=rep(NA, length=base::length(df01)),
                                  mean.time.on.bout=rep(NA, length=base::length(df01)),
                                  mean.time.off.bout=rep(NA, length=base::length(df01)))
  
  # loop to fill table of results
  ## if only 2 days
  if (base::length(df01)==2){final.loop <- 2
  } else {                              ## if more than 2 days...
    final.loop <- base::length(df01)-1}
  
  # for each day
  for (k in 1:final.loop) {
    # selecting working day
    df00 <- df01[[k]] 
    data.final$date[k] <- base::as.character(base::unique (df00$date))  # working date
    
    # selecting presence after the first off-bout
    vector.presence00 <- df00[[vector.incubation]]
    vector.index <- base::seq(1, to= length(df00[[vector.incubation]]), by=1)
    vector.presence01 <- base::data.frame(base::cbind (vector.index, vector.presence00))
    
    # first off-bout of the day
    for (p in 1:base::nrow(vector.presence01)) {
      if (vector.presence01$vector.presence00[p] == 0){
        starting.point <- vector.presence01$vector.index[p]
        break()
      } else {next()}
    }
    
    # last on-bout of the day
    for (p in 1:base::nrow(vector.presence01)) {
      if (base::rev(vector.presence01$vector.presence00)[p] == 0){
        ending.point <- base::rev(vector.presence01$vector.index)[p]
        break()
      } else {next()}
    }
    
    # selecting the window between first off-bout and last on-bout
    presence.seq <- vector.presence01[vector.index >= starting.point &
                                        vector.index <= ending.point+1, ]
    presence.seq$index2 <- base::seq(1, to=base::nrow(presence.seq), by= 1)
    # 
    # calculation of the number of on and off bouts, using loops and 
    # temporaly variables
    start.loop <- 1
    if (utils::tail(presence.seq$vector.index, 1) == utils::tail(vector.presence01$vector.index, 1)) {
      next()
    }
    while (utils::tail(start.loop, 1) < base::nrow(presence.seq)) {
      ind <- utils::tail(start.loop, 1)
      state <- presence.seq$vector.presence00[ind]
      
      for (j in (ind+1):base::nrow(presence.seq)) {
        if (presence.seq$vector.presence00[j] != state) {
          end <- presence.seq$index2[j]
          break()   
        } else {next()}
      }
      start.loop <- base::c(start.loop, end)
    }
    
    # the "start.loop" vector contains the info on length and number of daily on/off bouts
    df08 <- base::data.frame(cbind (start.loop, seq(1, to=length(start.loop), by=1)))
    
    if (base::length(start.loop) < 3){next()}
    offbout.index <- base::seq(1, to=base::length (start.loop), 2)
    onbout.index <- base::seq(2, to=base::length (start.loop)-1, 2)
    
    data.final$number.off.bouts[k] <- base::length(start.loop [offbout.index])
    data.final$number.on.bouts[k] <- base::length(start.loop [onbout.index])
    
    # 
    # length of off- bouts
    temp.var1 <- NA
    for (l in 1:base::length(offbout.index)) {
      temp.var1[l] <- df08$start.loop [df08$V2==offbout.index[l]+1] - start.loop [offbout.index[l]]
    }
    data.final$mean.time.off.bout[k] <-  base::mean (temp.var1, na.rm=TRUE) * sampling.rate
    
    # 
    # length of on- bouts
    temp.var2 <- NA
    for (l in 1:base::length(onbout.index)) {
      temp.var2[l] <- df08$start.loop [df08$V2==onbout.index[l]+1] - start.loop [onbout.index[l]]
    }
    data.final$mean.time.on.bout[k] <-  base::mean (temp.var2, na.rm=TRUE) * sampling.rate
  }
  return (data.final)
}
