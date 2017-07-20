#' An example data set of environmental temperatures to test
#' the use of \code{link{incRenv}}.
#' 
#' A dataset containing environmental temperatures for the study area
#' where the data in \code{\link{incR_rawdata}} were collected. 
#' This raw data set 
#' was produced by an iButton device (Maxim Integrated).
#' 
#' @format A data frame with 1570 rows and two variables, representing two days of 
#' environmental temperature recordings at two different locations in one study site.
#' \describe{
#'   \item{DATE}{a date-time column. Both elements, date and time, are concatenated in one column.}
#'   \item{env_temperature}{environmental temperature recordings.}
#'   }
#' @details use this data set to try out \code{link{incRenv}} after the very first
#' application of \code{\link{incRprep}} using \code{\link{incR_rawdata}}.
"incR_envdata"