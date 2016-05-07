#' An example of incubation temperature time-series after 
#' the use of \code{link{incRprep}}.
#'
#' A dataset containing temperatures, time and date for a blue tit nest. 
#' It also includes variables added by \code{\link{incRprep}} and the
#' and column with environmental temperature-
#' The raw data set \code{\link{incRdataExample2}}
#' 
#' @format A data frame with 954 rows and 10 variables, representing three days of nest
#' temperatures.
#' \describe{
#'   \item{DATE}{a date + time column. Both elements are concatenated in one column.}
#'   \item{valueT}{temperature recordings.}
#'   \item{index}{running number from first to last observation.}
#'   \item{time}{}
#'   \item{hour}{}
#'   \item{year}{}
#'   \item{date}{}
#'   \item{dec.time}{time in decimal hours.}
#'   \item{temp1}{difference between the \emph{ith} and the \emph{ith-1} 
#'   temperature recording.}
#'   \item{env.temp}{environmetal temperature, calculated per as hour average}
#' }
#' @details see \code{\link{incRprep}} for more details on the variables of this data set.
"incRdataExample2"