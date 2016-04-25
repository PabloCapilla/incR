#' @title Calculation of temperature average and variation for customised
#' time windows.
#' @description Calculation of temperature average and variation between two customised
#' time periods per day. Originally, this function was thought to calculate temperature
#' of incubation between day and night. 'Day' and 'night' being defined either by the user, 
#' by activity times or by civil twilight times.
#' @param data: data frame containing a time-series vector of 1s and 0s, where "1"
#' means "incubating individual inside nest" and "0" means "incubating individual 
#' outside the nests". This vector, 
#' under the name of "inc.vector", is provided by \code{\link{incR.scan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param limits: vector of length = 2 giving the time limits for calculaitons. For example,
#' 'c(6,20)" would calculate temperature averages and variation for two time periods, from 6 to 20
#' and from 20 to 6 of the next day. 'civil.twilight' and 'activiy.times' must be
#' FALSE to allow the use of 'limits'.
#' @param coor: coordenates for the location where temperature was recorded. 
#' When 'civil.twilight' is TRUE, 'coor' allows to define sunrise and sunset times
#' based on the \code{\link{crepuscule}} function (in maptools package). 
#' @param civil.twilight: TRUE or FALSE. Set as TRUE when time periods for calculation
#' are to be defined by civil twilight times - calculated using \code{\link{crepuscule}}.If
#' 'civil.twilight = TRUE', 'coor' and 'time.zone' need to be specified.
#' @param activity.times: TRUE or FALSE. Set as TRUE when time periods for calculation
#' are defined by \code{\link{incR.activity}}.
#' @param time.zone: time zone for \code{\link{crepuscule}} dawn and dusk calculations.
#' @return a data frame containing onset and end of activity times for each day in \emph{data}.
#' @author Pablo Capilla
#' @examples
#' To be included
#' @seealso \code{\link{incR.prep}} \code{\link{incR.scan}} \code{\link{incR.activity}}
#' \code{\link{crepuscule}}
#' @export 

incR.t <- function (data, 
                    limits=NULL, 
                    coor, 
                    activity.times=FALSE, 
                    civil.twilight=FALSE, 
                    time.zone=NULL) {
  # checking whether there is a date column
  if (base::is.null(data$date)) {
    stop("No column with name 'date' found")
  }
  
  # data by days
  data.onoff.act <- data
  df01 <- base::split (data, data$date)
  
  # defining table to write results in
  data.final <- base::data.frame (date=rep(NA, length=base::length(df01)),
                                  day.mean=rep(NA, length=base::length(df01)),
                                  day.var=rep(NA, length=base::length(df01)),
                                  night.mean=rep(NA, length=base::length(df01)),
                                  night.var=rep(NA, length=base::length(df01)))
  
  # this function allows three options to calculate nest temperatures between
  # two periods of time within 24hrs.
  # (1) you specify the time window you want
  ## to compute day and night mean and variation; 
  # (2) it takes onset and offset activity using incR.activity; or,
  # (3) uses civil twilight times to define night 
  # times to define day and night periods and calculate variation and temperature.
  #
  # First I create a table which specifies such periods depending on 1, 2 or 3.
  if (activity.times==TRUE) {
    # calculates onset and offset activity times
    act.times <- incR.activity (data= data.onoff.act, 
                                vector.incubation="inc.vector")
    if (base::is.null(data.onoff.act$inc.vector)) {
      stop ("Your incubation.vector for incR.activity is not named 'inc.vector',
              please, change its name to 'inc.vector' or specify it in
              incR.t adding an argument as in incR.activity: 'incubation.vector= '")
    }
    act.times$index <- base::seq (1,to=base::nrow(act.times), by=1)
  } else {
    if (civil.twilight==TRUE) {
      if (base::is.null(time.zone)|| base::is.null(coor)) {
        stop ("Time zone and/or coor not specified; please, do it by passing the argument
              to the incR.t function")
      }
      # calculates civil twilinght times
      dawn <- stats::na.omit(maptools::crepuscule(crds=base::matrix(c(coor[1], coor[2]), nrow=1), 
                                                  dateTime=base::as.POSIXct (base::as.character(base::unique (data$date)), 
                                                                             tz=time.zone),
                                                  solarDep=6, direction="dawn", POSIXct.out=TRUE))
      dawn$day_frac <- NULL
      dawn$dusk <-  stats::na.omit(maptools::crepuscule(base::matrix(c(coor[1], coor[2]), nrow=1), 
                                              base::as.POSIXct (base::as.character(base::unique (data$date)), 
                                                                tz=time.zone),
                                              solarDep=6, direction="dusk", POSIXct.out=TRUE))[,2]
      names(dawn) <- c("dawn.time", "dusk.time")
      
      
      # re-calculating decimal hours
      dawn$onset <- base::sapply(base::strsplit(base::strftime (dawn$dawn.time, 
                                                              format= "%H:%M", 
                                                              tz=time.zone),":"),
                               function(x) {
                                 x <- base::as.numeric(x)
                                 x[1]+x[2]/60
                               })
      dawn$offset <- base::sapply(base::strsplit(base::strftime (dawn$dusk.time, 
                                                              format= "%H:%M", 
                                                              tz=time.zone),":"),
                               function(x) {
                                 x <- base::as.numeric(x)
                                 x[1]+x[2]/60
                               })
      dawn$date <- base::as.character(as.POSIXlt(base::as.character(dawn$dawn.time), 
                                   tz = time.zone, format= "%Y-%m-%d"))
      act.times <- dawn
      act.times$index <- base::seq (1,to=base::nrow(act.times), by=1)
      
    } else {
      if (length(limits) < 2) {stop ("Please specify 'limits'")} # are there limits available?
      act.times <- base::data.frame (date=base::as.character(base::names(df01)),
                                     onset=rep(limits[1], length=base::length(df01)),
                                     onset=rep(limits[2], length=base::length(df01)))
      
      base::names(act.times)<- c("date", "onset", "offset")
      act.times$index <- base::seq (1,to=base::nrow(act.times), by=1)
        
    }
  }
    
    # loop to fill table of results
  for (k in 1:(base::length(df01))) {
    
    # each day one by one
    df00 <- df01[[k]]     
    data.final$date[k] <- base::as.character(base::unique(df00$date))
    
      # based on the calculations above, day and night mean and var in temperature are
      # calculated. Time limits for the calculations are needed
      index <- act.times [act.times$date==base::as.character(base::unique (df00$date)), c("index")]
      day.morning <- act.times [act.times$date==base::as.character(base::unique (df00$date)), c("onset")]
      day.evening <- act.times [act.times$date==base::as.character(base::unique (df00$date)), c("offset")]
      night.evening <- act.times [act.times$date==base::as.character(base::unique (df00$date)), c("offset")]
      night.morning <- act.times [act.times$index==index+1, c("onset")]
      
      # once the limits are set, the correct time points need to be selected 
      # 
      # DAY CALCULATIONS
      #
      day.data <- df00[df00$dec.time > day.morning & df00$dec.time < day.evening, ]
      data.final$day.mean[k] <- base::mean (day.data$valueT, na.rm=TRUE)
      data.final$day.var[k] <- stats::var (day.data$valueT, na.rm=TRUE)
      # 
      # NIGHT CALCULATIONS
      #
      # selecting time
      # computing number after sorting the data by time limits
      if (base::length(night.morning) > 0) { # is there one day ahead for calculations?
        # selecting night window
        if (night.evening < 24 && night.evening < night.morning) {
          subset.data <- df00 [df00$dec.time > night.evening & df00$dec.time < night.morning, ]
        } else {
          if (night.evening < 24 && night.evening > night.morning) {
            subset.nightBefore <- df00 [df00$dec.time > night.evening & df00$dec.time < 24, ]
            date.after <- base::unique (df00$date)+1
            day.after <- df01[[base::as.character(date.after)]] 
            subset.morning <- day.after [day.after$dec.time > 0 & day.after$dec.time < night.morning, ]
            subset.data <- base::rbind(subset.nightBefore, subset.morning)
          } 
        }
        data.final$night.mean[k] <- base::mean( subset.data$valueT, na.rm=TRUE)
        data.final$night.var[k] <- stats::var( subset.data$valueT, na.rm=TRUE)
        } else {
        data.final$night.mean[k] <- NA
        data.final$night.var[k] <- NA
      }
  }
  return (data.final)
  
}
