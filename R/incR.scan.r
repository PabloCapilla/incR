#' @title Calculation of incubation behaviour.
#' @description This is the core algorithm of incR and classifies time points as 1s or 0s depeding on
#' whether or not the incubating individual is estimated to be on the eggs. 
#' The algorithm
#' uses night variation to calibrate itself to temperature variation when the incubating
#' individual is (assumed to be) on the eggs. 
#' Therefore, a major assumption of this algorithm is that
#' there is a period of time in which temperature can be assumed to be constant or
#' representative of time windows of effective incubation. This time window is defined by
#' two arguments: lower.time and upper.time. The function is optimised to work using
#' a data frame produced by incR_prep.
#' 
#' The algorithm can be adapted to different environmental circunstances. Its
#' performance has been evaluated in several species and geographic areas, but calibration
#' using pilot data is recommended. 
#' @param data: ny
#' @param lower.time: ny
#' @param upper.time: ny
#' @param env.data: ny
#' @param sensitivity: ny
#' @param time.dif: ny
#' @param MAX.T: ny
#' @param maxNightVar_accepted: ny
#' 
#' @return 
#' 
#' @examples
#' 
#' @seealso \code{\link{incR_prep}}


incR_scan <- function (data=data, 
                       lower.time=22,
                       upper.time=3,
                       env.data=FALSE,
                       sensitivity=0.15,
                       time.dif=20,
                       MAX.T=38,
                       maxNightVar_accepted=2) {
  # NEW INTERNAL VECTORS
  # 
  # big list to store the final vector of female position (in or out the nest)
  female.list <- as.list(NA)  
  # lists and vectors used within the function
  list.threshold <- as.list(NA)
  vector.days <- as.vector(NA)  
  # final list to store all the data frame that the function returns
  incubation.list <- as.list(NA)
  
  # splits data by date
  list.day <- split (data, data$date)
  
  # selects the defined night time window
  if (lower.time < 24 && lower.time < upper.time) {
    subset.data <- data [data$dec.time > lower.time & data$dec.time < upper.time, ]
    subset.list <- split (subset.data, subset.data$date)
    
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
      subset.data <- rbind(subset.nightBefore, subset.morning)
      
      
      # splitting by effec.date
      subset.list <- split (subset.data, subset.data$effec.date)
      
    } 
  }
  # 
  # FUNCTION ITSELF - THROUGH DAY LOOPS
  #
  # loop to analyse each day separately
  for (d in 1:length(list.day)) {
    data.day <- list.day [[d]]
    
    # if there is no night ref for any day, then warning message and jump
    # to next day
    if (sort(names (subset.list)==as.character(unique(data.day$date)),
             decreasing=TRUE)[1]==TRUE) {
      subset.night <-  subset.list[[as.character(unique(data.day$date))]]
    } else {
      warning(paste("No night reference period for ",
                    as.character(unique(data.day$date)), 
                    " - day skipped"))
      old.leaving.threshold <- NA
      old.entering.threshold <- NA
      final.leaving.threshold <- NA
      final.entering.threshold <- NA
      night_day_varRatio <- NA
      list.threshold[[d]] <- c(as.character(unique (data.day$date)), 
                               old.entering.threshold, 
                               final.entering.threshold,
                               old.leaving.threshold, 
                               final.leaving.threshold, 
                               night_day_varRatio)
      next()
    }
    
    # checking for night variation
    # if much variation, then the night is not used
    night.drop <- min (subset.night$temp1, na.rm=TRUE)       # super off-bout
    night.raise <- max (subset.night$temp1, na.rm=TRUE)      # super on-bout
    if (night.drop <= -maxNightVar_accepted ||
        night.raise >= +maxNightVar_accepted) {
      warning (paste("Night variation on ", paste(
        as.character(unique(subset.night$date)[1]),
        as.character(unique(subset.night$date)[2]), sep="/"),
        " has passed set limit"))
      
      if (d==1) {
        warning (paste ("Night drop limit exit on ", paste(
          as.character(unique(subset.night$date)[1]),
          as.character(unique(subset.night$date)[2]), sep="/"),
          " and no previous night as reference. Day not analysed."))
        old.entering.threshold <- NA
        old.leaving.threshold <- NA
        final.entering.threshold <- NA
        final.leaving.threshold <- NA 
        night_day_varRatio <-  NA
        list.threshold[[d]] <- c(as.character(unique (data.day$date)), 
                                 old.entering.threshold, 
                                 final.entering.threshold,
                                 old.leaving.threshold, 
                                 final.leaving.threshold, 
                                 night_day_varRatio)
        next()
      } else {
        old.entering.threshold <- max (subset.night$temp1, na.rm=TRUE)
        old.leaving.threshold <- min (subset.night$temp1, na.rm=TRUE)
        if (is.na(list.threshold[[d-1]][3]) || is.na(list.threshold[[d-1]][5])){
          warning (paste ("Night drop limit exit on ", paste(
            as.character(unique(subset.night$date)[1]),
            as.character(unique(subset.night$date)[2]), sep="/"),
            " and no previous night as reference. Day not analysed."))
          final.entering.threshold <- NA
          final.leaving.threshold <- NA 
          night_day_varRatio <-  var (subset.night$temp1, na.rm=TRUE) / 
            var(data.day$temp1[data.day$dec.time>11 & data.day$dec.time<15], na.rm=TRUE)
          list.threshold[[d]] <- c(as.character(unique (data.day$date)), 
                                   old.entering.threshold, 
                                   final.entering.threshold,
                                   old.leaving.threshold, 
                                   final.leaving.threshold, 
                                   night_day_varRatio)
          next()
        } else {
          final.entering.threshold <- as.numeric(list.threshold[[d-1]][3])
          final.leaving.threshold <- as.numeric(list.threshold[[d-1]][5])
          night_day_varRatio <-  var (subset.night$temp1, na.rm=TRUE) / 
            var(data.day$temp1[data.day$dec.time>11 & data.day$dec.time<15], na.rm=TRUE)
        }
      }
    } else {
      # maximum/minimum change in temperature at "night"
      old.entering.threshold <- NA
      old.leaving.threshold <- NA
      final.entering.threshold <- max (subset.night$temp1, na.rm=TRUE)
      final.leaving.threshold <- min (subset.night$temp1, na.rm=TRUE)
      night_day_varRatio <-  var (subset.night$temp1, na.rm=TRUE) / 
        var(data.day$temp1[data.day$dec.time>11 & data.day$dec.time<15], na.rm=TRUE)
    }
    
    # storing data
    list.threshold[[d]] <- c(as.character(unique (data.day$date)), 
                             old.entering.threshold, 
                             final.entering.threshold,
                             old.leaving.threshold, 
                             final.leaving.threshold, 
                             night_day_varRatio)
    
    # assessment of differential temperature
    for (i in 2:length(data.day$temp1)) {
      
      data.day$leaving[1] <- 0
      data.day$entering[1] <- 1
      
      # distance from nest temperature to env.temperature
      if (is.na(data.day$valueT[i])) {next()}
      # is there environmental data?
      if (env.data==TRUE) {
        statement <-  (data.day$valueT[i] - data.day$env.temp[i]) < time.dif
      } else {
        if (MAX.T==NULL) {stop ("No maximum temperature assigned")
        } else {
          statement <-  (MAX.T - data.day$valueT[i]) > time.dif
        }
      }
      # now the evaluation of statement
      if (statement) {
        correction.min <- sensitivity
        correction.max <- 1
      } else {
        correction.min <- 1
        correction.max <- 0
      }
      
      # sorting of off-bouts
      if (is.na(data.day$temp1[i])) {next()}
      if (data.day$temp1[i] < final.leaving.threshold*correction.min) {
        data.day$leaving[i] <- 1
      } else {
        data.day$leaving[i] <- 0
      }
      
      # sorting of on-bouts
      if (data.day$temp1[i] > (final.entering.threshold*correction.max)) {
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
    if (nrow(data.leaving)==0) {
      data.day$inc.vector <- 1
      next()
    }
    
    #
    # based on when the female left and entered the nest, a vector of the
    # presence of the female in the nest can be created
    #
    #big loop for clasification
    for (j in 1:length(data.day$entering)) {
      
      # only loop through especial cases
      if (j != tail(seq.loop, 1)) {next()}
      # filling the gaps
      if (data.day$entering[j]==1) {
        index.dif <- data.leaving$index - data.day$index[j]
        
        # if there's no more changes ahead, fill and stop
        if (max(index.dif)<=0) {
          data.day$inc.vector[j:length(data.day$entering)] <- 1
          break ()
        }
        # gap to fill
        addition <- min(index.dif [index.dif > 0])
        new.index <-  j +  addition
        
        # filling 1's when female is in
        data.day$inc.vector [(j):(new.index-1)] <- 1
        
        # new loop step
        seq.loop <- c(seq.loop, tail(seq.loop, 1)+addition)
        
      } else {
        # repetition for off-bout times
        if (data.day$leaving[j]==1) {
          index.dif <- data.entering$index - data.day$index[j]
          
          # stop when at the end of the vector
          if (max(index.dif)<=0) {
            data.day$inc.vector[j:length(data.day$entering)] <- 0
            break ()
          }
          # gap to fill
          addition <- min(index.dif [index.dif > 0])
          new.index <-  j + addition
          
          # filling 0's when female is not in
          data.day$inc.vector [j:(new.index-1)] <- 0
          
          # new loop step
          seq.loop <- c(seq.loop, tail(seq.loop, 1)+addition)
        }
      }
    }
    
    # compiling list
    female.list[[d]] <- data.day
  }
  # data with the new vector for in/out female
  final.data <- do.call("rbind",female.list)
  incubation.list[[1]] <-  final.data
  
  final.threshold <- as.data.frame(do.call("rbind",list.threshold))
  names(final.threshold) <- c("date", 
                              "old.entering.threshold", 
                              "final.entering.threshold",
                              "old.leaving.threshold", "final.leaving.threshold", 
                              "night_day_varRatio")
  incubation.list[[2]] <- final.threshold[complete.cases(final.threshold$date),]
  incubation.list[[2]]$year <- unique (data[["year"]])
  
  return(incubation.list)
}
