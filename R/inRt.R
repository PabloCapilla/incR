#' @title Calculation of temperature average and variance for customised
#' time windows
#' @description Calculation of temperature average and variation between two customised
#' time periods per day. Time windows can be defined by the user using the \code{limits} argument,
#' defined by bird activity time (using the \code{activity.times} parameter) or set accoring to
#' twilight times if coordenates are provided (\code{coor}).
#' @param data data frame containing a time-series vector of 1's and 0's (incubation scores), 
#' where "1"
#' means "incubating individual inside nest" and "0" means "incubating individual 
#' outside the nests". This vector, 
#' under the name of \code{incR_score}, is provided by \code{\link{incRscan}} in the 
#' first object of the returned list. A column named "date" is needed to refer to daily
#' calculations.
#' @param temp.name (character object) name of the column containing temperature data 
#' in data. 
#' @param limits vector of length 2 giving the time limits for calculations. For example,
#' 'c(6,20)' would calculate temperature averages and variances for two time periods, from 6 to 20
#' and from 20 to 6 of the next day. 'civil.twilight' and 'activity.times' must be
#' FALSE to allow the use of 'limits'.
#' @param coor coordinates for the location where temperature was recorded,
#' formatted as decimal degrees N/S, decimal degrees E/W.
#' When 'civil.twilight' is TRUE, 'coor' allows the user to define sunrise and sunset times
#' based on the \code{\link{crepuscule}} function (in \code{maptools} package). 
#' @param civil.twilight TRUE or FALSE. Set as TRUE when time periods for calculation
#' are to be defined by civil twilight times - calculated using \code{\link{crepuscule}}. 
#' If 'civil.twilight = TRUE', 'coor' and 'time.zone' need to be specified.
#' @param activity.times TRUE or FALSE. Set as TRUE when time periods for calculation
#' are defined by \code{\link{incRact}}. Data must contain a column named 
#' 'incR_score' for the use of \code{\link{incRact}}.
#' @param time.zone time zone for \code{\link{crepuscule}} dawn and dusk calculations.
#' @param ... use parameters in \code{\link{incRact}} if \emph{activity.times} = TRUE.
#' @return a data frame containing temperature means and variance for the defined time 
#' window.
#' @author Pablo Capilla-Lasheras
#' @examples
#' # loading example data
#' data(incR_procdata)
#' 
#' # calculation based on chosen times from 6am to 7pm and 7pm to 6am
#' incRt (data=incR_procdata, 
#'         temp.name="temperature",
#'         limits=c(6,19), 
#'         coor=NULL, 
#'         civil.twilight=FALSE, 
#'         activity.times=FALSE,
#'         time.zone=NULL)
#'         
#' # calculation based on activity times
#' incRt (data=incR_procdata, 
#'         temp.name="temperature", 
#'         limits=NULL, 
#'         coor=NULL, 
#'         civil.twilight=FALSE, 
#'         activity.times=TRUE,
#'         time.zone=NULL,
#'         time_column="time",             # extra argument needed for incRact
#'         vector.incubation="incR_score") # extra argument needed for incRact
#'         
#' # calculation based on civil twilight
#' incRt (data=incR_procdata, 
#'         temp.name="temperature",
#'         limits=NULL, 
#'         coor=c(42,0.89), 
#'         civil.twilight=TRUE, 
#'         activity.times=FALSE,
#'         time.zone="GMT")
#' @seealso \code{\link{incRprep}} \code{\link{incRscan}} \code{\link{incRact}}
#' \code{\link{crepuscule}}
#' @export 

incRt <- function (data, 
                   temp.name,
                   limits=NULL, 
                   coor=NULL, 
                   activity.times=FALSE, 
                   civil.twilight=FALSE, 
                   time.zone=NULL, 
                   ...) {
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
  # (2) it takes first_offbout and last_onbout activity using incRact; or,
  # (3) uses civil twilight times to define night 
  # times to define day and night periods and calculate variation and temperature.
  #
  # First I create a table which specifies such periods depending on 1, 2 or 3.
  if (activity.times==TRUE) {
    # calculates first_offbout and last_onbout activity times
    act.times <- incRact (data= data.onoff.act, 
                               ...)
    act.times$first_offbout <- do.call(args = base::lapply(strsplit(act.times$first_offbout, " "), 
                                                           FUN = function(x) {
                                                             time <- lubridate::hm(x)
                                                             lubridate::hour(time) + lubridate::minute(time)/60
                                                           }),
                                       what = "rbind")
    
    
    act.times$last_onbout <- do.call(args = base::lapply(strsplit(act.times$last_onbout, " "), 
                                                           FUN = function(x) {
                                                             time <- lubridate::hm(x)
                                                             lubridate::hour(time) + lubridate::minute(time)/60
                                                           }),
                                       what = "rbind")
  
    act.times$index <- base::seq (1,to=base::nrow(act.times), by=1)
  } else {
    if (civil.twilight==TRUE) {
      if (base::is.null(time.zone)|| base::is.null(coor)) {
        stop ("Time zone and/or coor not specified; please, do it by passing the argument
              to the incRt function")
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
      dawn$first_offbout <- do.call(args = base::lapply(base::as.list(dawn$dawn.time), 
                                                        FUN = function(x) {
                                                          time <- lubridate::ymd_hms(x)
                                                          lubridate::hour(time) + lubridate::minute(time)/60
                                                        }),
                                    what = "rbind")
      dawn$last_onbout <- do.call(args = base::lapply(base::as.list(dawn$dusk.time), 
                                                        FUN = function(x) {
                                                          time <- lubridate::ymd_hms(x)
                                                          lubridate::hour(time) + lubridate::minute(time)/60
                                                        }),
                                    what = "rbind")
      dawn$date <- base::as.character(as.POSIXlt(base::as.character(dawn$dawn.time), 
                                                 tz = time.zone, format= "%Y-%m-%d"))
      act.times <- dawn
      act.times$index <- base::seq (1,to=base::nrow(act.times), by=1)
      
    } else {
      if (length(limits) < 2) {stop ("Please specify 'limits'")} # are there limits available?
      act.times <- base::data.frame (date=base::as.character(base::names(df01)),
                                     first_offbout=rep(limits[1], length=base::length(df01)),
                                     last_onbout=rep(limits[2], length=base::length(df01)))
      
      base::names(act.times)<- c("date", "first_offbout", "last_onbout")
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
    day.morning <- base::as.numeric(act.times[act.times$date==base::as.character(base::unique (df00$date)), 
                             c("first_offbout")])
    day.evening <- base::as.numeric(act.times [act.times$date==base::as.character(base::unique (df00$date)), 
                              c("last_onbout")])
    night.evening <- base::as.numeric(act.times [act.times$date==base::as.character(base::unique (df00$date)), 
                                c("last_onbout")])
    night.morning <- base::as.numeric(act.times[act.times$index==index+1, c("first_offbout")])
    # once the limits are set, the correct time points need to be selected 
    # 
    # DAY CALCULATIONS
    #
    if (is.null(df00$dec_time)) {
      stop("'dec_time' column is missing. Please, use exactly that name.")
      }
    day.data <- df00[df00$dec_time > day.morning & df00$dec_time < day.evening, ]
    data.final$day.mean[k] <- base::mean (day.data[[temp.name]], na.rm=TRUE)
    data.final$day.var[k] <- stats::var (day.data[[temp.name]], na.rm=TRUE)
    # 
    # NIGHT CALCULATIONS
    #
    # selecting time
    # computing number after sorting the data by time limits
    if (base::length(night.morning) > 0) { # is there one day ahead for calculations?
      # selecting night window
      if (night.evening < 24 && night.evening < night.morning) {
        subset.data <- df00 [df00$dec_time > night.evening & df00$dec_time < night.morning, ]
      } else {
        if (night.evening < 24 && night.evening > night.morning) {
          subset.nightBefore <- df00 [df00$dec_time > night.evening & df00$dec_time < 24, ]
          date.after <- base::unique (df00$date)+1
          day.after <- df01[[base::as.character(date.after)]] 
          subset.morning <- day.after [day.after$dec_time > 0 & day.after$dec_time < night.morning, ]
          subset.data <- base::rbind(subset.nightBefore, subset.morning)
        } 
      }
      data.final$night.mean[k] <- base::mean(subset.data[[temp.name]], na.rm=TRUE)
      data.final$night.var[k] <- stats::var(subset.data[[temp.name]], na.rm=TRUE)
    } else {
      data.final$night.mean[k] <- NA
      data.final$night.var[k] <- NA
    }
  }
  return (data.final)
  
}
