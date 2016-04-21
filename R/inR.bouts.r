#' @title Calculation of the number of daily on- and off-bouts
#' @description Calculation of 
#' @param data ny
#' @param vector ny
#' @param sampling.rate ny
#' @return ny
#' @author Pablo Capilla
#' @examples
#' To be included
#' @seealso \code{\link{incR.prep}} \code{\link{incR.scan}} \code{\link{incR.activity}}
#' \code{\link{incR.constancy}}

incR.bouts <- function (data, vector, sampling.rate) {
  # splitting df by day
  df01 <- split (data, data$date)
  
  # defining table to write results in
  data.final <- data.frame (date=rep(NA, length=length(df01)),
                            number.on.bouts=rep(NA, length=length(df01)),
                            number.off.bouts=rep(NA, length=length(df01)),
                            mean.time.on.bout=rep(NA, length=length(df01)),
                            mean.time.off.bout=rep(NA, length=length(df01)))
  
  # loop to fill table of results
  ## if only 2 days
  if (length(df01)==2){final.loop <- 2
  } else {                              ## if more than 2 days...
    final.loop <- length(df01)-1}
  
  # for each day
  for (k in 1:final.loop) {
    # selecting working day
    df00 <- df01[[k]] 
    data.final$date[k] <- as.character(unique (df00$date))  # working date
    
    # selecting presence after the first off-bout
    vector.presence00 <- df00[[vector]]
    vector.index <- seq(1, to= length(df00[[vector]]), by=1)
    vector.presence01 <- data.frame(cbind (vector.index, vector.presence00))
    
    # first off-bout of the day
    for (p in 1:nrow(vector.presence01)) {
      if (vector.presence01$vector.presence00[p] == 0){
        starting.point <- vector.presence01$vector.index[p]
        break()
      } else {next()}
    }
    
    # last on-bout of the day
    for (p in 1:nrow(vector.presence01)) {
      if (rev(vector.presence01$vector.presence00)[p] == 0){
        ending.point <- rev(vector.presence01$vector.index)[p]
        break()
      } else {next()}
    }
    
    # selecting the window between first off-bout and last on-bout
    presence.seq <- vector.presence01[vector.index >= starting.point &
                                        vector.index <= ending.point+1, ]
    presence.seq$index2 <- seq(1, to=nrow(presence.seq), by= 1)
    # 
    # calculation of the number of on and off bouts, using loops and 
    # temporaly variables
    start.loop <- 1
    if (tail(presence.seq$vector.index, 1) == tail(vector.presence01$vector.index, 1)) {
      next()
    }
    while (tail(start.loop, 1) < nrow(presence.seq)) {
      ind <- tail(start.loop, 1)
      state <- presence.seq$vector.presence00[ind]
      
      for (j in (ind+1):nrow(presence.seq)) {
        if (presence.seq$vector.presence00[j] != state) {
          end <- presence.seq$index2[j]
          break()   
        } else {next()}
      }
      start.loop <- c(start.loop, end)
    }
    
    # the "start.loop" vector contains the info on length and number of daily on/off bouts
    df08 <- data.frame(cbind (start.loop, seq(1, to=length(start.loop), by=1)))
    
    if (length(start.loop) < 3){next()}
    offbout.index <- seq(1, to=length (start.loop), 2)
    onbout.index <- seq(2, to=length (start.loop)-1, 2)
    
    data.final$number.off.bouts[k] <- length(start.loop [offbout.index])
    data.final$number.on.bouts[k] <- length(start.loop [onbout.index])
    
    # 
    # length of off- bouts
    temp.var1 <- NA
    for (l in 1:length(offbout.index)) {
      temp.var1[l] <- df08$start.loop [df08$V2==offbout.index[l]+1] - start.loop [offbout.index[l]]
    }
    data.final$mean.time.off.bout[k] <-  mean (temp.var1, na.rm=TRUE) * sampling.rate
    
    # 
    # length of on- bouts
    temp.var2 <- NA
    for (l in 1:length(onbout.index)) {
      temp.var2[l] <- df08$start.loop [df08$V2==onbout.index[l]+1] - start.loop [onbout.index[l]]
    }
    data.final$mean.time.on.bout[k] <-  mean (temp.var2, na.rm=TRUE) * sampling.rate
  }
  return (data.final)
}
