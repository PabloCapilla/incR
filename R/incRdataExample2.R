#' A second example of incubation temperature time-series after 
#' the use of \code{link{incRprep}}.
#'
#' A dataset containing temperatures, time and date for a blue tit nest. This data set
#' was produced by an iButton® device (Maxim) and represents raw data to start the analysis
#' of incubation behaviour. Environmental temperature have been added as average per hour.
#' 
#' @format A data frame with 709 rows and 3 variables, representing three days of nest
#' temperatures.
#' \describe{
#'   \item{DATE}{a date + time column. Both elements are concatenated in one column}
#'   \item{valueT}{temperature recordings}
#'   \item{env.temp}{environmental temperature}
#' }
"incRdataExample2"