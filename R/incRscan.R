#' @title Calculation of incubation behaviour
#' @description This is the core algorithm of \code{incR} and classifies time points as 1s or 0s depending on
#' whether or not the incubating individual is estimated to be on the eggs. 
#' The algorithm
#' uses night variation to daily-calibrate itself to temperature variation when the incubating
#' individual is (assumed to be) on the eggs. 
#' Therefore, a major assumption of this algorithm is that
#' there is a period of time in which temperature can be assumed to be constant or
#' representative of time windows of effective incubation. This time window is defined by
#' two arguments: lower.time and upper.time. The function is optimised to work using
#' a data frame produced by \code{\link{incRprep}}.
#' 
#' In the near future, extended functionality will be included, especially including
#' environmental temperature information into analysis. The
#' performance of this function has been evaluated in several bird species and geographic areas, 
#' but calibration using pilot data is always recommended. 
#' 
#' @param data data frame for analysis. It must contained four columns named as follow:
#' 'date', 'temp1', 'dec.time' and 'index', where 'temp1' is the difference between
#' the \emph{ith} and  \emph{ith-1} temperature recordings; 'dec.time' is time in
#' decimal hours; and index is a running number from 1 to \emph{N}, N being the 
#' total number of observations. \code{\link{incRprep}} returns a data frame with
#' this variables and the correct names, ready to be passed through \emph{incRscan}.
#' @param lower.time lower limit of time window for calibration (numeric).
#' @param upper.time upper limit of time window for calibration (numeric).
#' @param sensitivity ratio of reduction in temperature threshold. When nest temperature
#' does not drop close to environmental temperatures, this value can be kept to 1. If 
#' nest temperature follows environmental temperature at any point, 
#' then adjustment of this value may
#' be required to detect short on/off-bouts at lower nest temperatures (see details).
#' @param temp.diff temperature difference between \emph{maxinc.Temp} and an observation which
#' triggers the sensitivity parameter.
#' @param maxNightVar_accepted maximum temperature variation between two consecutive points
#' within the calibrating window that is accepted. If this variation value is surpassed, 
#' calibratinng window is discarded and a previous one is used for calibration.
#' @param env.temp name of column for environmental temperatures.
#' @return 
#' The function returns a list with two objects. The first object is the original
#' data frame with an extra column named 'inc.vector'. This vector is formed by 1s and 0s,
#' representing whether the incubating individual is (1) or outside the nest (0).
#' 
#' The second object is a data frame with one day per row. Four columns tell the user
#' the thresholds employed to estimate incubating individual behaviour. A fifth column accounts
#' for the ratio between the calibrating window temperature variation and the variation in temperature 
#' between 11am and 3pm. The lower this value the more clear the pattern between night and day
#' variation. It may serve the user as an indication of the signal / noise ratio in the analysed
#' data set.
#' @section Details:
#' Details of the algorithmic calculation 
#' See package vignette for a description of how this function works. This
#' section will be updated soon.
#' @author Pablo Capilla
#' @examples
#' #' # loading example data
#' data(incRdataExample)
#' # first incRprep prepares the data
#' new.data <- incRprep (data=incRdataExample,
#'                        date.name= "DATE",
#'                        date.format= "%d/%m/%Y %H:%M",
#'                        timezone="GMT",
#'                        temperature.name="valueT")
#' # then the data frame is ready for incRscan                      
#' incubation.analysis <- incRscan (data=new.data, 
#'                                   lower.time=22,
#'                                   upper.time=3,
#'                                   sensitivity=0.15,
#'                                   temp.diff=5,
#'                                   maxNightVar_accepted=2,
#'                                   env.temp="env.temp")
#' inc.data <- incubation.analysis[[1]]
#' inc.thresholds <- incubation.analysis[[2]]
#' @seealso \code{\link{incRprep}} \code{\link{incRconstancy}} \code{\link{incRactivity}}
#' @export 
incRscan <- function (data, 
                       lower.time,
                       upper.time,
                       sensitivity=0.15,
                       temp.diff=8,
                       maxNightVar_accepted=2,
                       env.temp) {
  ##### CHECKING THE PRESENCE OF APPROPRIATE COLUMN NAMES #####
  if (base::is.null(data$date) || base::is.null(data$dec.time) || base::is.null(data$temp1) || base::is.null(data$index)){
    stop("Please, check that the columns 'date', 'dec.time', 'temp1' and 'index' exist in your data frame")
  }
  
  ##### NEW INTERNAL VECTORS AND LISTS #####
  # big list to store the final vector of inc. individual position (in or out the nest)
  incubation.list <- base::as.list(NA)  
  # lists and vectors used within the function
  threshold.list <- base::as.list(NA)
  vector.days <- base::as.vector(NA)  
  # splits data by date
  list.day <- base::split (data, data$date)
  
  # final list to return
  incubation.final <- base::as.list(NA)
  ##### SELECTING NIGHT TIME WINDOW #####
  # selects the defined night time window
  if (lower.time < 24 && lower.time < upper.time) {
    subset.data <- data [data$dec.time > lower.time & data$dec.time < upper.time, ]
    subset.list <- base::split (subset.data, subset.data$date)
  } else {
    if (lower.time < 24 && lower.time > upper.time) {
      # night period
      subset.nightBefore <- data [data$dec.time > lower.time & data$dec.time < 24, ]
      ## matching night periods with next day's morning
      subset.nightBefore$effec.date <- subset.nightBefore$date + 1
      # day period
      subset.morning <- data [data$dec.time > 0 & data$dec.time < upper.time, ]
      ## replicating new date column
      subset.morning$effec.date <- subset.morning$date
      # combining both periods
      subset.data <- base::rbind(subset.nightBefore, subset.morning)
      # splitting by effec.date
      subset.list <- base::split (subset.data, subset.data$effec.date)
      
    } 
  }
  ##### FUNCTION ITSELF - THROUGH DAY LOOPS #####
  # loop to analyse each day separately
  for (d in 1:base::length(list.day)) {
    data.day <- list.day [[d]]
    # if there is no night ref for any day, then warning message and jump
    # to next day
    if (base::sort(base::names (subset.list)==base::as.character(base::unique(data.day$date)),
             decreasing=TRUE)[1]==TRUE) {
      subset.night <-  subset.list[[base::as.character(base::unique(data.day$date))]]
    } else {
      warning(base::paste("No night reference period for ",
                          base::as.character(unique(data.day$date)), 
                    " - day skipped"))
      old.maxDrop <- NA
      old.maxIncrease <- NA
      final.maxDrop <- NA
      final.maxIncrease <- NA
      night_day_varRatio <- NA
      threshold.list[[d]] <- c(as.character(unique (data.day$date)), 
                               old.maxIncrease, 
                               final.maxIncrease,
                               old.maxDrop, 
                               final.maxDrop, 
                               night_day_varRatio)
      next()
    }
    
    # checking for night variation
    # if much variation, then the night is not used
    night.drop <- base::min (subset.night$temp1, na.rm=TRUE)       # super off-bout
    night.raise <- base::max (subset.night$temp1, na.rm=TRUE)      # super on-bout
    if (night.drop <= -maxNightVar_accepted ||
        night.raise >= +maxNightVar_accepted) {
      warning (base::paste("Night variation on ", base::paste(
        base::as.character(unique(subset.night$date)[1]),
        base::as.character(unique(subset.night$date)[2]), sep="/"),
        " has passed set limit"))
      
      if (d==1) {
        warning (base::paste ("Night drop limit exit on ", base::paste(
          base::as.character(base::unique(subset.night$date)[1]),
          base::as.character(base::unique(subset.night$date)[2]), sep="/"),
          " and no previous night as reference. Day not analysed."))
        old.maxIncrease <- NA
        old.maxDrop <- NA
        final.maxIncrease <- NA
        final.maxDrop <- NA 
        night_day_varRatio <-  NA
        threshold.list[[d]] <- c(base::as.character(base::unique (data.day$date)), 
                                 old.maxIncrease, 
                                 final.maxIncrease,
                                 old.maxDrop, 
                                 final.maxDrop, 
                                 night_day_varRatio)
        next()
      } else {
        old.maxIncrease <- base::max (subset.night$temp1, na.rm=TRUE)
        old.maxDrop <- base::min (subset.night$temp1, na.rm=TRUE)
        if (base::is.na(threshold.list[[d-1]][3]) || base::is.na(threshold.list[[d-1]][5])){
          warning (base::paste ("Night drop limit exit on ", base::paste(
            base::as.character(base::unique(subset.night$date)[1]),
            base::as.character(base::unique(subset.night$date)[2]), sep="/"),
            " and no previous night as reference. Day not analysed."))
          final.maxIncrease <- NA
          final.maxDrop <- NA 
          night_day_varRatio <-  stats::var (subset.night$temp1, na.rm=TRUE) / 
            stats::var(data.day$temp1[data.day$dec.time>11 & data.day$dec.time<15], na.rm=TRUE)
          threshold.list[[d]] <- base::c(base::as.character(base::unique (data.day$date)), 
                                   old.maxIncrease, 
                                   final.maxIncrease,
                                   old.maxDrop, 
                                   final.maxDrop, 
                                   night_day_varRatio)
          next()
        } else {
          final.maxIncrease <- base::as.numeric(threshold.list[[d-1]][3])
          final.maxDrop <- base::as.numeric(threshold.list[[d-1]][5])
          night_day_varRatio <-  stats::var (subset.night$temp1, na.rm=TRUE) / 
            stats::var(data.day$temp1[data.day$dec.time>11 & data.day$dec.time<15], na.rm=TRUE)
        }
      }
    } else {
      # maximum/minimum change in temperature at "night"
      old.maxIncrease <- NA
      old.maxDrop <- NA
      final.maxIncrease <- base::max (subset.night$temp1, na.rm=TRUE)
      final.maxDrop <- base::min (subset.night$temp1, na.rm=TRUE)
      night_day_varRatio <-  stats::var (subset.night$temp1, na.rm=TRUE) / 
        stats::var(data.day$temp1[data.day$dec.time>11 & data.day$dec.time<15], na.rm=TRUE)
    }
    
    # storing data
    threshold.list[[d]] <- base::c(base::as.character(base::unique (data.day$date)), 
                             old.maxIncrease, 
                             final.maxIncrease,
                             old.maxDrop, 
                             final.maxDrop, 
                             night_day_varRatio)
    
    # assessment of differential temperature
    for (i in 2:base::length(data.day$temp1)) {
      data.day$leaving[1] <- 0  # initial state
      data.day$entering[1] <- 1 # initial state
      
      # distance from nest temperature to env.temperature
      if (base::is.na(data.day$valueT[i])) {next()}
      # is there environmental data?
      if (is.null(env.temp)){
        stop("Provide the name of the column with environmental temperatures")
      }
      statement <-  base::abs((data.day$valueT[i] - data.day[[env.temp]][i])) < temp.diff
      if (statement) {
        correction.min <- sensitivity
        correction.max <- 1
      } else {
        correction.min <- 1
        correction.max <- 0
      }

      # sorting of off-bouts
      if (base::is.na(data.day$temp1[i])) {next()}
      if (data.day$temp1[i] < final.maxDrop*correction.min) {
        data.day$leaving[i] <- 1
      } else {
        data.day$leaving[i] <- 0
      }
      
      # sorting of on-bouts
      if (data.day$temp1[i] > (final.maxIncrease*correction.max)) {
        data.day$entering[i] <- 1
      } else {
        data.day$entering[i] <- 0
      }
    }
    
    # variables for loop
    data.day$inc.vector <- NA
    seq.loop <- 1
    
    # data set for further loops; they select "leaving" and "entering" events
    data.leaving <- data.day[data.day$leaving==1,]
    data.entering <- data.day[data.day$entering==1,]
    
    # if female did not leave, then always in
    if (base::nrow(data.leaving)==0) {
      data.day$inc.vector <- 1
      next()
    }
    
    #
    # based on when the female left and entered the nest, a vector of the
    # presence of the female in the nest can be created
    #
    #big loop for clasification
    for (j in 1:base::length(data.day$entering)) {
      
      # only loop through especial cases
      if (j != utils::tail(seq.loop, 1)) {next()}
      # filling the gaps
      if (data.day$entering[j]==1) {
        index.dif <- data.leaving$index - data.day$index[j]
        
        # if there's no more changes ahead, fill and stop
        if (base::max(index.dif)<=0) {
          data.day$inc.vector[j:base::length(data.day$entering)] <- 1
          break ()
        }
        # gap to fill
        addition <- base::min(index.dif [index.dif > 0])
        new.index <-  j +  addition
        
        # filling 1's when female is in
        data.day$inc.vector [(j):(new.index-1)] <- 1
        
        # new loop step
        seq.loop <- base::c(seq.loop, utils::tail(seq.loop, 1)+addition)
        
      } else {
        # repetition for off-bout times
        if (data.day$leaving[j]==1) {
          index.dif <- data.entering$index - data.day$index[j]
          
          # stop when at the end of the vector
          if (base::max(index.dif)<=0) {
            data.day$inc.vector[j:base::length(data.day$entering)] <- 0
            break ()
          }
          # gap to fill
          addition <- base::min(index.dif [index.dif > 0])
          new.index <-  j + addition
          
          # filling 0's when female is not in
          data.day$inc.vector [j:(new.index-1)] <- 0
          
          # new loop step
          seq.loop <- base::c(seq.loop, utils::tail(seq.loop, 1)+addition)
        }
      }
    }
    
    #filling night windows unless night var exit
    if (night.drop > -maxNightVar_accepted ||
        night.raise < +maxNightVar_accepted) {
      data.day$inc.vector[data.day$dec.time < upper.time] <- 1
      data.day$inc.vector[data.day$dec.time > lower.time] <- 1
    } 
    
    # compiling list
    incubation.list[[d]] <- data.day
  }
  # data with the new vector for in/out female
  final.data <- base::do.call("rbind",incubation.list)
  # transitory columns are deleted
  final.data$entering <- NULL
  final.data$leaving <- NULL
  incubation.final[[1]] <-  final.data
  
  final.threshold <- base::as.data.frame(base::do.call("rbind",threshold.list))
  base::names(final.threshold) <- c("date", 
                              "old.maxIncrease", 
                              "final.maxIncrease",
                              "old.maxDrop", "final.maxDrop", 
                              "night_day_varRatio")
  incubation.final[[2]] <- final.threshold[stats::complete.cases(final.threshold$date),]
  incubation.final[[2]]$year <- base::unique (data[["year"]])
  
  return(incubation.final)
}
