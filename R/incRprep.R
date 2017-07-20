#' @title Data preparation for incubation analysis in incR
#' @description Preparing incubation time series for further analysis.
#' This function takes a data file containing a temporal series of temperature recordings
#' and adds some the extra variables needed to use
#' further functions embedded in the incR package. 
#' It simply accommodates a raw data frame, reformatting date and time columns
#' automatically.
#' @param data raw data from incubation time series. It must contain a column 
#' with date and time information for each observation (e.g. "2017-05-01 21:01"). 
#' The function is written to
#' handle date and time concatenated in one unique column (see example below).
#' @param date.name name of the date and time column
#' @param date.format format for date and time column. 
#' It must be a character object as specified in the function \code{\link{strptime}}.   
#' \code{\link{incRprep}} assumes that the date and time column contains date and time, 
#' If date and time are in different columns, please, concatenate them in one
#' column before running the function.
#' @param timezone time zone for time calculations. See \code{\link{strptime}}.
#' documentation for more details.
#' @param  temperature.name name of the column storing temperature information.
#' @return The original data frame with additional columns for:
#' \enumerate{
#' \item index: a running number identifying every row in the data set.
#' \item dec_time: time in decimal hours (e.g. "22:30" becomes 22.5). 
#' \item time: in  'H:M' format.
#' \item hour: in 'H' format.
#' \item minute: in 'M' format.
#' \item date: in  'Y-m-d' format.
#' \item temp1: difference between the \emph{i}th temperature value and the \emph{i-1} one.
#' }
#' @author Pablo Capilla-Lasheras
#' @examples
#' # loading example data
#' data(incubation_rawdata)
#' new.data <- incRprep (data=incR_rawdata,
#'                        date.name= "DATE",
#'                        date.format= "%d/%m/%Y %H:%M",
#'                        timezone="GMT",
#'                        temperature.name="temperature")
#' head (new.data, 3)
#' @export 
incRprep <- function (data, 
                      date.name,
                      date.format, 
                      timezone,
                      temperature.name) {
  # checking for correct column names
  ## date name
  if (date.name=="date"){
    stop("Incorrect date name, please do not use 'date'")
  }
  ### re-formating date and time and generating new time columns
  # index for each row
  data$index <- base::seq(1, to=base::length(data[[temperature.name]]), by=1)
  # converting time and date in different formats
  dt <- base::strptime(data[[date.name]], format=date.format, tz=timezone)
  data$time <- strftime (dt, format= "%H:%M")
  # variables for hour and year
  data$hour <- base::as.numeric(base::format (dt, "%H"))
  data$minute <- base::as.numeric(base::format (dt, "%M"))

  # date
  data$date <- base::as.Date(base::format(dt,"%Y-%m-%d"))
  # time in min decimals
  data$dec_time <- base::sapply(base::strsplit(data$time,":"),
                                function(x) {
                                  x <- base::as.numeric(x)
                                  x[1]+x[2]/60
                                })
  # diferential temperatures
  # loop to calculate t - (t-1)
  # t - t-1
  data[["temp1"]] <- NA
  for (i in 2:length(data[[temperature.name]])) {
    data$temp1[1] <- NA
    data$temp1[i] <- data[[temperature.name]][i] - data[[temperature.name]][i-1]
  }
  
  return (data)
}
