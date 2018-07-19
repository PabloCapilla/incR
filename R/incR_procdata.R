#' An example of incubation temperature time-series after 
#' the use of \code{\link{incRprep}} and \code{\link{incRenv}}.
#'
#' A dataset containing temperatures, time and date for a blue tit nest. 
#' It also includes variables added by \code{\link{incRprep}} 
#' and \code{\link{incRenv}}.
#' The raw data set \code{\link{incR_rawdata}}
#' was produced by an iButton device (Maxim) and represents raw data to start the analysis
#' of incubation behaviour
#' 
#' @format A data frame with 954 rows and 11 variables, representing two days of nest
#' temperatures.
#' \describe{
#'   \item{DATE}{a date-time column. Both elements, date and time, are concatenated in one column.}
#'   \item{temperature}{nest temperature recordings.}
#'   \item{index}{running number from first to last observation.}
#'   \item{time}{}
#'   \item{hour}{}
#'   \item{minute}{}
#'   \item{second}{}
#'   \item{date}{}
#'   \item{dec_time}{time in decimal hours.}
#'   \item{temp1}{difference between the \emph{ith} and the \emph{ith-1} 
#'   nest temperature recording.}
#'   \item{env_temp}{environmental temperature, calculated per as hour average using
#'   \code{\link{incRenv}}.}
#'   \item{incR_score}{Incubation scores (0/1) as calculated by \code{\link{incRscan}} }

#' }
#' @details see \code{\link{incRprep}} and \code{\link{incRenv}}for more details on the variables of this data set
#' and how it was produced.
"incR_procdata"