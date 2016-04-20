#' @title Data manupilation for incubation data analysis
#' @description Preparing incubation time series for further analysis. 
#' Initial data manipulation.
#' This function takes a data file containing a temporal series of temperature
#' and adds all the extra variables needed to use
#' further functions embedded in the incR package. 
#' It simply acommodates the data frame reformating dates, times and 
#' so on.
#' @param data: raw data from incubation time series. This must include two columns with the
#' following names:
#' \itemize{
#'  \item DATE - date and time collapsed in one columns, eg. "dd/mm/yyyy hh:mm". 
#'  Its format must be specified in \emph{format} (see below).
#'  \item valueT - temperature values.
#' }
#' @param format: format for DATE as defined in the function \emph{strptime}.   
#' \emph{incR_prep} assumes the DATE contains date and time, so choose
#' its format accordingly.
#' @param timezone: time zone for time calculations. See \emph{strptime}
#' documentation for more details.
#' @return The original data frame with additional colunms for:
#' \enumerate{
#' \item index: a running number identifying every row in the data set.
#' \item time: time in DATE and "hour"
#' \item date: date in DATE and "year"
#' \item dec.time: time in decimal format
#' \item temp1 and temp2: differential temperatures defines as
#' \deqn{T[i] - T[i-1]} and  \deqn{T[i] - T[i-2]}.
#' }
#' @examples
#' NULL

incR_prep <- function (data, date.format, timezone) 
{
  # converting time and date into the right formats
  t <- strptime(data$DATE, format=date.format, tz=timezone)
  data$time <- strftime (t, format= "%H:%M")
  data$date <- as.Date(format(data$t,"%Y-%m-%d"))
  
  # index for each row
  data$index <- seq(1, to=length(data$valueT), by=1)
  
  # variables for hour and year
  data$hour <- as.numeric(format (data$t, "%H"))
  data$year <- as.numeric(format(data$t, "%Y")) 
  
  # time in min decimals
  tempo <- strftime(data$t, format= "%H:%M", tz=timezone)
  data$dec.time <- sapply(strsplit(tempo,":"),
                          function(x) {
                            x <- as.numeric(x)
                            x[1]+x[2]/60
                          })
  
  # diferential temperatures
  # loop to calculate t - (t-1) and -(t-2)
  # t - t-1
  loop1 <- length(data$valueT)
  loop2 <- length(data$valueT)
  data$temp1 <- NA
  data$temp2 <- NA
  
  # differentialteperature order 1
  for (i in 2:loop1) {
    data$temp1[1] <- NA
    data$temp1[i] <- data$valueT[i]-data$valueT[i-1]
  }
  
  # differentialteperature order 2
  for (i in 3:loop2) {
    data$temp2[1:2] <- NA
    data$temp2[i] <- data$valueT[i]-data$valueT[i-2]
  }
  
  # new data frame with the name you want to have
  return (data)
  
}
