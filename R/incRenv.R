#' @title Matching environmental and nest temperatures
#' @description
#' This function takes a data frame with recordings of environmental temperature and another
#' with nest temperatures and merges both by time. The user can do this work manually, however, 
#' \code{\link{incRenv}} is thought to
#' automate data preparation (in combination with \code{\link{incRprep}}) to use 
#' \code{\link{incRscan}} after. Code to update and improve this function provided by Natalie van Dis.
#' @param data.nest data frame containing nest temperature recordings. It must have two compulsory
#' columns 'date' and 'hour' displaying dates and the hour of each observation. These two columns
#' are provided if the user uses \code{\link{incRprep}} before. 
#' @param data.env data frame containing environmental temperatures to be merged with nest temperature
#' records. Please, provide date and time of each observation in one unique column as requested 
#' for \code{\link{incRprep}}.
#' @param env.temperature.name name of the column containing temperature recordings in the 
#' \emph{data.env} data frame.
#' @param env.date.name name of the column containing date and time in the 
#' \emph{data.env} data frame.
#' @param env.date.format format of \emph{env.date.name}. Similar to \code{\link{incRprep}}.
#' @param env.timezone time zone of the environmental recordings. Similar to \code{\link{incRprep}}.
#' @return The original \emph{data.nest} with an additional column for hour-averaged environmental
#' temperature. This new variable is thought to serve as \emph{env_temp} in
#' \code{\link{incRscan}}.
#' @details This function is thought to be used after \code{\link{incRprep}} as it uses some of the
#' additional variables created by \code{\link{incRprep}}.
#' @author Pablo Capilla-Lasheras
#' @examples
#' data(incR_envdata)  # environmental data
#' head (incR_envdata)
#' 
#' data(incR_rawdata)  # loading nest data
#' head (incR_rawdata)
#' 
#' # the first step in to format the raw data using incRprep
#' new.data <- incRprep (data=incR_rawdata,
#'                       date.name= "DATE",
#'                       date.format= "%d/%m/%Y %H:%M",
#'                       timezone="GMT",
#'                       temperature.name="temperature")
#'                       
#' # then use incRenv to merge environmental data
#' new.data2 <- incRenv (data.nest = new.data,
#'                       data.env = incR_envdata, 
#'                       env.temperature.name = "env_temperature", 
#'                       env.date.name = "DATE", 
#'                       env.date.format = "%d/%m/%Y %H:%M", 
#'                       env.timezone = "GMT")
#'                      
#' head (new.data2, 3)
#' @seealso \code{\link{incRprep}} \code{\link{incRscan}}
#' @export 
incRenv <- function (data.nest,
                       data.env, 
                       env.temperature.name, 
                       env.date.name,
                       env.date.format, 
                       env.timezone) {
  
  # data reading and creating variables
  data.env$t <- base::strptime(data.env[[env.date.name]], 
                               format=env.date.format, 
                               tz=env.timezone)
  data.env$time <-  base::strftime (data.env$t, format= "%H:%M")
  data.env$date <-  base::as.Date(base::format(data.env$t,"%Y-%m-%d"))
  data.env$index <-  base::seq(1, to= base::nrow(data.env), by=1)
  data.env$hour <-  base::as.numeric(base::format (data.env$t, "%H"))
  # necessary list
  time.temp.list <- base::list(NA)
  
  # average temperatures per hour
  day_hour <- stats::aggregate(env_temperature ~ date + hour, FUN = mean, data = data.env)
  names(day_hour) <- c("date", "hour", "env_temp")
  
  ######
  ###### matching env.temp with the inside-nest data
  ##
  # stop if there is no data and no hour in data.nest
  if (base::is.null(data.nest[["date"]])){
    stop("Provide a column in data.nest under the name 'date' containing
         the date of each observation")
  }
  
  if ( base::is.null(data.nest[["hour"]])){
    stop("Provide a column in data.nest under the name 'hour' containing
         the hour of each observation")
  }
  
  # actual calculation
  data.nest <- base::merge(data.nest, day_hour, by=c("date", "hour"))
  return(data.nest)
}

