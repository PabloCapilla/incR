#' @title Scoring of incubation based on a fixed temperature threshold
#' @description Modified version of \code{incRscan}. Data points are assigned to incubation on/off-bout based on
#' a fixed temperature threshold for the difference in nest temperature expected when an off-bout starts.
#' @param data data frame for analysis. It must contained four columns named as follow:
#' \code{date}, \code{temp1}, \code{dec_time} and \code{index}, where \code{temp1} is the difference between
#' the \emph{ith} and  \emph{i-1th} temperature recordings; \code{dec_time} is time in
#' decimal hours; and \code{index} is a running number from 1 to \emph{N}, \emph{N} being the 
#' total number of observations. \code{\link{incRprep}} returns a data frame with
#' these variables and the correct names, ready to be passed through \code{incRscan}.
#' @param temp.name (character object) name of the column containing temperature data 
#' in \code{data}. 
#' @param sensitivity percentage of reduction in temperature threshold. When nest temperature
#' does not drop close to environmental temperature, this value can be kept to 1. If 
#' nest temperature follows environmental temperature at any point, 
#' then, adjustment of this value may
#' be required to detect short on/off-bouts at lower nest temperatures (see details).
#' @param temp.diff.threshold threshold for temperature difference between \code{env.temp} and an observation which
#' triggers application of the sensitivity parameter. 
#' @param env.temp name of a column containing environmental temperatures.
#' @param temperature_threshold Maximum temperature difference between two consecutive nest temperature recordings 
#' allowed for an on-bout. When the difference in nest temperature between two consecutive recordings is higher than
#' this value, an off-bout is detected. 
#' @return The function returns the original data frame with a new column named 'incR_score'. 
#' This new variable is formed by 1's and 0's,
#' representing whether the incubating individual is inside ("1") or outside the nest ("0").
#' #' @section Details:
#' See \code{\link{incRenv}} for more details
#' @author Pablo Capilla-Lasheras
#' @examples
#' # incR_procdata is a data frame processed by incRprep and incRenv.
#' # It contains suitable information to run incRscan
#' data(incR_procdata)
#' 
#' incubation.analysis <- incRscan_v2 (data=incR_procdata, 
#'                                   temp.name="temperature",
#'                                   temperature_threshold = 0.75,
#'                                   sensitivity=0.15,
#'                                   temp.diff.threshold=5,
#'                                   env.temp="env_temp")
#' @seealso \code{\link{incRprep}} \code{\link{incRenv}} \code{\link{incRscan}} 
#' @export 
incRscan_v2 <- function (data, 
                         temp.name, 
                         sensitivity, 
                         temperature_threshold,
                         temp.diff.threshold, 
                         env.temp) {
  
  if (base::is.null(data$date) || base::is.null(data$dec_time) || 
      base::is.null(data$temp1) || base::is.null(data$index)) {
    stop("Please, check that the columns 'date', 'dec_time', 'temp1' and 'index' exist in your data frame")
  }
  
  # set up list to store results
  incubation.list <- base::as.list(NA)
  threshold.list <- base::as.list(NA)
  vector.days <- base::as.vector(NA)
  list.day <- base::split(data, data$date)
  
  # loop over each day
  for (d in 1:base::length(list.day)) {
    data.day <- list.day[[d]]
    
    
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
      if (data.day$temp1[i] < (temperature_threshold) * -1 * correction.min) {
        data.day$leaving[i] <- 1
      }else {
        data.day$leaving[i] <- 0
      }
      if (data.day$temp1[i] > (temperature_threshold * correction.max)) {
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
      incubation.list[[d]] <- data.day
    }
  }
  final.data <- base::do.call("rbind", incubation.list)
  final.data$entering <- NULL
  final.data$leaving <- NULL
  return(final.data)
}