#' @title Data manipulation for incubation data analysis
#' @description Preparing incubation time series for further analysis. 
#' Initial data manipulation.
#' This function takes a data file containing a temporal series of temperature
#' and adds some the extra variables needed to use
#' further functions embedded in the incR package. 
#' It simply acommodates the raw data frame, reformating dates and times 
#' automatically.
#' @param data: raw data from incubation time series. It must contain a column 
#' with date + time information for each observation. The function is written to
#' handle date and time concatenated in one unique column (see example below).
#' @param date.name: name of the date + time column
#' @param date.format: date and time format for date + time column. 
#' It must be a character object as specified in the function \emph{strptime}.   
#' \emph{incR.prep} assumes the the date + time column contains date and time, 
#' If date and time are in different columns, please concatenate them in one
#' column before running the function (see example below).
#' its format accordingly.
#' @param timezone: time zone for time calculations. See \emph{strptime}
#' documentation for more details.
#' @param  temperature.name: name of the column storing temperature information.
#' @return The original data frame (data) with additional colunms for:
#' \enumerate{
#' \item index: a running number identifying every row in the data set.
#' \item dec.time: time in decimal hours. 
#' \item time: in  'H:M' format.
#' \item hour: in 'H' format.
#' \item year: in 'Y' format.
#' \item date: in  'Y-m-d' format.
#' \item temp1: difference between the ith temperature value (Ti) and the previous one (Ti-1).
#' }
#' @author Pablo Capilla
#' @examples
#' To be included
#' 
#' @importFrom base length seq as.Date as.numeric  strftime
#' @export 
incR.prep <- function (data, date.name,
                       date.format, timezone,
                       temperature.name) {
  # checking for correct column names
  ## date name
  if (date.name=="date"){
    stop("Incorrect date name, please do not use 'date'")
  }
  ### re-formating date and time and generating new time columns
  # index for each row
  data$index <- seq(1, to=length(data$valueT), by=1)
  # converting time and date in different formats
  dt <- strptime(data[[date.name]], format=date.format, tz=timezone)
  data$time <- strftime (dt, format= "%H:%M")
  # variables for hour and year
  data$hour <- as.numeric(format (data$time, "%H"))
  data$year <- as.numeric(format(data$time, "%Y")) 
  # date
  data$date <- as.Date(format(dt,"%Y-%m-%d"))
  # time in min decimals
  data$dec.time <- sapply(strsplit(data$time,":"),
                          function(x) {
                            x <- as.numeric(x)
                            x[1]+x[2]/60
                          })
  
  # diferential temperatures
  # loop to calculate t - (t-1) and -(t-2)
  # t - t-1
  loop1 <- length(data[["temperature.name"]])
  temperature.original <- data[["temperature.name"]]
  data[["temp1"]] <- NA
  for (i in 2:loop1) {
    data$temp1[1] <- NA
    data$temp1[i] <- temperature.original[i]-temperature.original[i-1]
  }

  return (data)
}
