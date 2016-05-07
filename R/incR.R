#' incR: Analysis of animal incubation
#' 
#' This packages is formed by a suite of R functions that help the user to get
#' useful biological information from raw time-series data of 
#' incubation temperatures. It is specially thought to be of interest for
#' the study of uni-parental or intermittent incubating species when incubation temperature is
#' higher than environmental temperature.
#' The package is written in R language to allow the potential
#' contribution of a wide R audience.  
#' 
#' @section Release notes and caveats:
#' The current version of \code{\link{incR}} is under active development.
#' Documentation and code are frequently updated and bugs are likely to be
#' found. If so, please, send me your recommendations.
#' 
#' In the near future:
#' \enumerate{
#' \item \code{\link{incRscan}} will store warning messages in an independent object.
#' \item \code{\link{incRbouts}} will be extended to produce off-bout-specific
#' data, giving temperature drop, duration and time per individual off-bout.
#' }
#' 
#' @section incR functions:
#' The current version of incR contains the following functions (plus two example data sets):
#' \code{\link{incRprep}}, \code{\link{incRscan}},
#' \code{\link{incRactivity}}, \code{\link{incRconstancy}}
#' \code{\link{incRbouts}}, \code{\link{incRt}}
#' 
#' @docType package
#' @name incR  
NULL