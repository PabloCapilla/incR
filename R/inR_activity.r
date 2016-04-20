#' @title Calculation of onset and end of daily activity.
#' @description This functins uses the vector of female presence in nest
#' to calculate onset of daily activity (first off-bout of a day),
#' and end of daily activity (last on-bout of a day). A column for dates, named "date"
#' is needed in the data argument.
#' @param data: data frame containing a time-series vector of 1 and 0, where "1"
#' means "females inside nests" and "0" means "female" off the nests. This vector, 
#' under the name of "inc.vector" is provided by \code{\link{incR_scan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param vector.presence: (character class) name of the vector containing information
#' for incubating individual presence/ausence of nests.
#' 
#' @return data frame containing a column (vector) for incubating individual presence/
#' ausence of nests.
#' 
#' @examples
#' 
#' @seealso \code{\link{incR_prep}} \code{\link{incR_scan}}



incR_activity <- function (data, vector.presence=NULL) {
  
  df01 <- split (data, data$date)
  data.final <- data.frame (date=rep(NA, length=length(df01)),
                            onset=rep(NA, length=length(df01)),
                            offset=rep(NA, length=length(df01)))
  for (k in 1:length(df01)) {
    
    df00 <- df01[[k]] 
    data.final$date[k] <- as.character(unique (df00$date))
    
    
    for (s in 1:length(df00[[vector.presence]])) {
      if (df00[[vector.presence]][s] == 0) {
        data.final$onset[k] <- df00$dec.time[s]
        break()
      } else {
        next()
      }
    }
    
    for (s in 1:length(rev(df00[[vector.presence]]))) {
      if (rev(df00[[vector.presence]])[s] == 0) {
        if (head(rev(df00[[vector.presence]]), 1)==0) {
          data.final$offset[k] <- NA
        } else {
          data.final$offset[k] <- rev(df00$dec.time)[s-1]
        }
        break()
      } else {
        next()
      }
    }
    
  }
  
  return(data.final)
}