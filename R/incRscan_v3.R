#' @title Scoring of incubation based on combination of dynamic (as in \code{\link{incRscan}}) and fixed temperature thresholds 
#' (as in \code{\link{incRscan_v2}}
#' @description Modified version of \code{incRscan}. It combines the assignment of incubation on/off-bout implemented in 
#' \code{\link{incRscan}} and \code{\link{incRscan_v2}}.
#' @param data data frame for analysis. It must contained four columns named as follow:
#' \code{date}, \code{temp1}, \code{dec_time} and \code{index}, where \code{temp1} is the difference between
#' the \emph{ith} and  \emph{i-1th} temperature recordings; \code{dec_time} is time in
#' decimal hours; and \code{index} is a running number from 1 to \emph{N}, \emph{N} being the 
#' total number of observations. \code{\link{incRprep}} returns a data frame with
#' these variables and the correct names, ready to be passed through \code{incRscan}.
#' @param temp.name (character object) name of the column containing temperature data 
#' in \code{data}. 
#' @param lower.time lower limit of time window for calibration (numeric).
#' @param upper.time upper limit of time window for calibration (numeric).
#' @param sensitivity percentage of reduction in temperature threshold. When nest temperature
#' does not drop close to environmental temperature, this value can be kept to 1. If 
#' nest temperature follows environmental temperature at any point, 
#' then, adjustment of this value may
#' be required to detect short on/off-bouts at lower nest temperatures (see details).
#' @param temp.diff deprecated. Use temp.diff.threshold.
#' @param temp.diff.threshold threshold for temperature difference between \code{env.temp} and an observation which
#' triggers application of the sensitivity parameter. 
#' @param env.temp name of a column containing environmental temperatures.
#' @param temperature_threshold Maximum temperature difference between two consecutive nest temperature recordings 
#' allowed for an on-bout. When the difference in nest temperature between two consecutive recordings is higher than
#' this value, an off-bout is detected. Only applies for days when a calibration window is not available or \code{temp.diff.threshold} is exceeded.
#' @return The function returns the original data frame with a new column named 'incR_score'. 
#' This new variable is formed by 1's and 0's,
#' representing whether the incubating individual is inside ("1") or outside the nest ("0").
#' #' @section Details:
#' See \code{\link{incRenv}} for more details
#' @author Pablo Capilla-Lasheras
#' @seealso \code{\link{incRscan}} \code{\link{incRscan_v2}}
#' @export 
incRscan_v3 <- function (data, 
                         temp.name, 
                         temperature_threshold, 
                         lower.time, 
                         upper.time, 
                         sensitivity, 
                         temp.diff,
                         temp.diff.threshold, 
                         maxNightVariation, 
                         env.temp) {
  
  # warnings if arguments are missing
  if (!missing("temp.diff")) {
    warning("argument deprecated. Use temp.diff.threshold instead")
  }
  
  if (base::is.null(data$date) || base::is.null(data$dec_time) || 
      base::is.null(data$temp1) || base::is.null(data$index)) {
    stop("Please, check that the columns 'date', 'dec_time', 'temp1' and 'index' exist in your data frame")
  }
  
  # set up list to store results
  incubation.list <- base::as.list(NA)
  threshold.list <- base::as.list(NA)
  vector.days <- base::as.vector(NA)
  list.day <- base::split(data, data$date)
  incubation.final <- base::as.list(NA)
  
  # find time windows for calibration
  if (lower.time < 24 && lower.time < upper.time) {
    subset.data <- data[data$dec_time > lower.time & data$dec_time < upper.time, ]
    subset.list <- base::split(subset.data, subset.data$date)
  } else {
    if (lower.time < 24 && lower.time > upper.time) {
      subset.nightBefore <- data[data$dec_time > lower.time & data$dec_time < 24, ]
      subset.nightBefore$effec.date <- lubridate::ymd(subset.nightBefore$date) + 1
      subset.morning <- data[data$dec_time > 0 & data$dec_time < upper.time, ]
      subset.morning$effec.date <- subset.morning$date
      subset.data <- base::rbind(subset.nightBefore, subset.morning)
      subset.list <- base::split(subset.data, subset.data$effec.date)
    }
  }
  
  # loop over each day
  for (d in 1:base::length(list.day)) {
    data.day <- list.day[[d]]
    
    # checking that there is calibration data for a given day
    
    
    if (base::sort(base::names(subset.list) == base::as.character(base::unique(data.day$date)), 
                   decreasing = TRUE)[1] == TRUE) { # if FALSE, on calibration window
      subset.night <- subset.list[[base::as.character(base::unique(data.day$date))]]
      
      # are there data in the calibration window
      if(nrow(subset.night) == 0){ # if TRUE - no calibration data
        print(base::paste("Fixed temperature threshold used for", 
                          base::as.character(unique(data.day$date)), 
                          "- temperature_threshold =",
                          temperature_threshold))
        first.maxDrop <- NA
        first.maxIncrease <- NA
        final.maxDrop <- temperature_threshold * -1
        final.maxIncrease <- temperature_threshold
        night_day_varRatio <- NA
        threshold.list[[d]] <- c(as.character(unique(data.day$date)), 
                                 first.maxIncrease, 
                                 final.maxIncrease, 
                                 first.maxDrop, 
                                 final.maxDrop, 
                                 night_day_varRatio)
        night.drop <- temperature_threshold * -1
        night.raise <- temperature_threshold
        
      } else {
        
        ## calculation of thresholds based on 'night' window
        # calculating temperature thresholds
        night.drop <- base::min(subset.night$temp1, na.rm = TRUE)
        night.raise <- base::max(subset.night$temp1, na.rm = TRUE)
        
        if (night.drop <= -maxNightVariation || night.raise >=  +maxNightVariation) {
          print(base::paste("Night variation on ", base::paste(base::as.character(unique(subset.night$date)[1]), 
                                                               base::as.character(unique(subset.night$date)[2]), 
                                                               sep = "/"), " has passed set limit"))
          if (d == 1) {
            print(base::paste("Night drop limit exit on ", 
                              base::paste(base::as.character(base::unique(subset.night$date)[1]), 
                                          base::as.character(base::unique(subset.night$date)[2]), 
                                          sep = "/"), 
                              " and no previous night as reference. Fixed temperature_threshold used",
                              "- temperature_threshold =",
                              temperature_threshold))
            first.maxDrop <- NA
            first.maxIncrease <- NA
            final.maxDrop <- temperature_threshold * -1
            final.maxIncrease <- temperature_threshold
            night_day_varRatio <- NA
            threshold.list[[d]] <- c(as.character(unique(data.day$date)), 
                                     first.maxIncrease, 
                                     final.maxIncrease, 
                                     first.maxDrop, 
                                     final.maxDrop, 
                                     night_day_varRatio)
            
            night.drop <- temperature_threshold * -1
            night.raise <- temperature_threshold
            
          }else {
            first.maxIncrease <- base::round(base::max(subset.night$temp1, 
                                                       na.rm = TRUE), digits = 3)
            first.maxDrop <- base::round(base::min(subset.night$temp1, 
                                                   na.rm = TRUE), digits = 3)
            if (base::is.na(threshold.list[[d - 1]][3]) || 
                base::is.na(threshold.list[[d - 1]][5])) {
              print(base::paste("Night drop limit exit on ", 
                                base::paste(base::as.character(base::unique(subset.night$date)[1]), 
                                            base::as.character(base::unique(subset.night$date)[2]), 
                                            sep = "/"), 
                                " and no previous night as reference. Fixed temperature_threshold used",
                                "- temperature_threshold =",
                                temperature_threshold))
              final.maxDrop <- temperature_threshold
              final.maxIncrease <- temperature_threshold
              night_day_varRatio <- base::round(stats::var(subset.night$temp1, 
                                                           na.rm = TRUE)/stats::var(data.day$temp1[data.day$dec_time > 
                                                                                                     11 & data.day$dec_time < 15], na.rm = TRUE), 
                                                digits = 3)
              threshold.list[[d]] <- base::c(base::as.character(base::unique(data.day$date)), 
                                             first.maxIncrease, 
                                             final.maxIncrease, 
                                             first.maxDrop, 
                                             final.maxDrop, 
                                             night_day_varRatio)
            } else {
              final.maxIncrease <- base::round(base::as.numeric(threshold.list[[d - 1]][3]), digits = 3)
              final.maxDrop <- base::round(base::as.numeric(threshold.list[[d - 1]][5]), digits = 3)
              night_day_varRatio <- base::round(stats::var(subset.night$temp1, 
                                                           na.rm = TRUE)/stats::var(data.day$temp1[data.day$dec_time > 11 & data.day$dec_time < 15], 
                                                                                    na.rm = TRUE), 
                                                digits = 3)
            }
          }
        } else {
          first.maxIncrease <- NA
          first.maxDrop <- NA
          final.maxIncrease <- base::round(base::max(subset.night$temp1,  na.rm = TRUE), 
                                           digits = 3)
          final.maxDrop <- base::round(base::min(subset.night$temp1, na.rm = TRUE), digits = 3)
          night_day_varRatio <- base::round(stats::var(subset.night$temp1, 
                                                       na.rm = TRUE)/stats::var(data.day$temp1[data.day$dec_time > 11 & data.day$dec_time < 15], 
                                                                                na.rm = TRUE), 
                                            digits = 3)
        }
        
        threshold.list[[d]] <- base::c(base::as.character(base::unique(data.day$date)), 
                                       first.maxIncrease, 
                                       final.maxIncrease, 
                                       first.maxDrop, 
                                       final.maxDrop, 
                                       night_day_varRatio)
        
        
        
        
        
        
      }
    } else {
      print(base::paste("Fixed temperature threshold used for", 
                        base::as.character(unique(data.day$date)), 
                        "- temperature_threshold =",
                        temperature_threshold))
      first.maxDrop <- NA
      first.maxIncrease <- NA
      final.maxDrop <- temperature_threshold * -1
      final.maxIncrease <- temperature_threshold
      night_day_varRatio <- NA
      threshold.list[[d]] <- c(as.character(unique(data.day$date)), 
                               first.maxIncrease, 
                               final.maxIncrease, 
                               -first.maxDrop, 
                               final.maxDrop, 
                               night_day_varRatio)
      
      night.drop <- temperature_threshold * -1
      night.raise <- temperature_threshold
    }
    
    
    ## classify points based on thresholds calculated above
    data.day$leaving <- NA
    data.day$entering <- NA
    data.day <- data.day[order(data.day$dec_time),]
    
    for (i in 2:base::length(data.day$temp1)) {
      data.day$leaving[1] <- 0
      data.day$entering[1] <- 1
      if (base::is.na(data.day[[temp.name]][i])) {
        (next)()
      }
      if (is.null(env.temp)) {
        stop("Provide the name of the column with environmental temperatures")
      }
      statement <- base::abs((data.day[[temp.name]][i] - 
                                data.day[[env.temp]][i])) < temp.diff.threshold
      if (statement) {
        correction.min <- sensitivity
        correction.max <- 1
      } else {
        correction.min <- 1
        correction.max <- 0
      }
      if (base::is.na(data.day$temp1[i])) {
        (next)()
      }
      if (data.day$temp1[i] < final.maxDrop * correction.min) {
        data.day$leaving[i] <- 1
      }else {
        data.day$leaving[i] <- 0
      }
      if (data.day$temp1[i] > (final.maxIncrease * correction.max)) {
        data.day$entering[i] <- 1
      }else {
        data.day$entering[i] <- 0
      }
    }
    data.day$incR_score <- NA
    seq.loop <- 1
    data.leaving <- data.day[data.day$leaving == 1, ]
    data.entering <- data.day[data.day$entering == 1, ]
    
    
    
    if (base::nrow(data.leaving) == 0) {
      data.day$incR_score <- 1
      incubation.list[[d]] <- data.day
    } else {
      for (j in 1:base::length(data.day$entering)) {
        if (j != utils::tail(seq.loop, 1)) {
          (next)()
        }
        if (data.day$entering[j] == 1) {
          index.dif <- data.leaving$index - data.day$index[j]
          if (base::max(index.dif) <= 0) {
            data.day$incR_score[j:base::length(data.day$entering)] <- 1
            (break)()
          }
          addition <- base::min(index.dif[index.dif > 0])
          new.index <- j + addition
          data.day$incR_score[(j):(new.index - 1)] <- 1
          seq.loop <- base::c(seq.loop, utils::tail(seq.loop,  1) + addition)
          
        }else {
          if (data.day$leaving[j] == 1) {
            index.dif <- data.entering$index - data.day$index[j]
            if (base::max(index.dif) <= 0) {
              data.day$incR_score[j:base::length(data.day$entering)] <- 0
              (break)()
            }
            addition <- base::min(index.dif[index.dif > 
                                              0])
            new.index <- j + addition
            data.day$incR_score[j:(new.index - 1)] <- 0
            seq.loop <- base::c(seq.loop, utils::tail(seq.loop, 1) + addition)
          } else {
            (next)()
          }
        }
      }
      if (night.drop > -maxNightVariation || night.raise < +maxNightVariation) {
        
        data.day$incR_score[data.day$dec_time < upper.time] <- 1
        data.day$incR_score[data.day$dec_time > lower.time] <- 1
      }
      incubation.list[[d]] <- data.day
    }
  }
  
  final.data <- base::do.call("rbind", incubation.list)
  final.data$entering <- NULL
  final.data$leaving <- NULL
  incubation.final[[1]] <- final.data
  final.threshold <- base::as.data.frame(base::do.call("rbind", 
                                                       threshold.list))
  base::names(final.threshold) <- c("date", "first.maxIncrease", 
                                    "final.maxIncrease", 
                                    "first.maxDrop", 
                                    "final.maxDrop", 
                                    "night_day_varRatio")
  incubation.final[[2]] <- final.threshold[stats::complete.cases(final.threshold$date),]
  names(incubation.final) <- c("incRscan_data", "incRscan_threshold")
  return(incubation.final)
}

