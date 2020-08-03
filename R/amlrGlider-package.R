#' Process AMLR glider data
#'
#' This package contains functions designed for processing AMLR glider data.
#' Currently this means converting AMLR glider output nc files to the nc
#' file format required by IOOS NGDAC
#'
#' @name swfscAirDAS-package
#' @aliases swfscAirDAS
#' @docType package
#' @title AMLR Glider Data Processing
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#'
#' @importFrom dplyr between
#' @importFrom ncdf4 nc_open nc_close ncvar_get nc_create ncdim_def ncvar_def ncatt_put ncvar_put
#' @importFrom stats na.omit
#' @importFrom utils head
#'
#' @keywords package
NULL
