#' An example data set with environmental temperatures to test
#' the use of \code{link{incRenv}}.
#' 
#' A dataset containing environmental temperatures for the study area
#' where the data in \code{\link{incRdataExample}} were collected. 
#' This raw data set 
#' was produced by an iButton® device (Maxim).
#' 
#' @format A data frame with 14227 rows and 3 variables, representing 30 days of 
#' environmental temperature recordings at two different locations in one study site.
#' \describe{
#'   \item{DATE}{a date + time column. Both elements are concatenated in one column.}
#'   \item{valueT}{environmental temperature recordings.}
#'   \item{BOX}{location of the iButton® employed for recording. Two iButtons® in the 
#'   vicinity of two nest boxes are listed.}
#'   }
#' @details use this data set to try out \code{link{incRenv}} after the very first
#' application of \code{\link{incRprep}}.
"incRenvironmentalData"