#' Extract deployment location
#'
#' Extract deployment location from AERD L1 data
#'
#' @param file.l1 file path for AMLR L1 nc file
#' @param n numeric; number of lat/lon positions to print. Default is 1
#' @param na.start.skip logical; indicates if \code{NA} lat/lon data should be
#'   removed from the front of the lat/lon data.
#'  Default is \code{TRUE}
#'
#' @details The purpose of this function is to extract the first one or more
#'   recorded positions to determine the approximate deployment location.
#'   This function extracts data for the variables 'latitude' and 'longitude'
#'   from \code{file.l1}. \code{file.l1} is expected to be an AMLR L1 file.
#'   For this function, 'front' (e.g. front rows with \code{NA} lat/lon data)
#'   means before the first record with non-\code{NA} lat and lon data.
#'
#' @return Data frame with 'lat' and 'lon' columns that contains the first \code{n} rows
#'   of latitude and longitude values, respecitvely, from \code{file.l1.}.
#'   If \code{na.start.skip} is \code{TRUE}, then front rows
#'   with \code{NA} lat or lon values are removed before returning the first \code{n} rows.
#'   See Details for a description of 'front'
#'
#'   A warning is printed if any of the printed coordiantes are not
#'   south of 58 deg S and between 70 deg W and 50 deg W.
#' @export
amlr_deployment_position <- function(file.l1, n = 1, na.start.skip = TRUE) {
  stopifnot(
    inherits(file.l1, "character"),
    file.exists(file.l1),
    inherits(n, c("numeric", "integer")),
    inherits(na.start.skip, "logical")
  )

  if (!grepl("L1", file.l1))
    warning("The phrase 'L1' is not in the L1 file name. Provided L1 file name (file.l1):\n",
            file.l1, immediate. = TRUE)


  x1 <- nc_open(file.l1)
  x1.lat <- ncvar_get(x1, "latitude")
  x1.lon <- ncvar_get(x1, "longitude")

  x1.ll <- data.frame(lat = as.numeric(x1.lat), lon = as.numeric(x1.lon))
  x1.ll.nona <- !is.na(x1.lat) & !is.na(x1.lon)

  if (na.start.skip) {
    na.skip <- sum(!x1.ll.nona[1:min(which(x1.ll.nona))])

    if (na.skip > 0)
      message("The first ", na.skip, " records had NA lat/lon points and thus were skipped")

    df.out <- head(x1.ll[x1.ll.nona, ], n)

  } else {
    df.out <- head(x1.ll, n)
  }

  if (any(!is.na(df.out$lat)))
    if (any(na.omit(df.out$lat) > -58))
      warning("At least one output lat value is > -58; are these data useable?")

  if (any(!is.na(df.out$lon)))
    if (any(!between(na.omit(df.out$lon), -70, -50)))
      warning("At least one output lon value is not between -70 and -50; are these data useable?")

  df.out
}
