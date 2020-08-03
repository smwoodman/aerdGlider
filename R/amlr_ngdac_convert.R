#' Convert AMLR L1 and L2 glider data to NGDAC format v2.0
#'
#' Convert AMLR L1 and L2 glider data to NGDAC format v2.0
#'
#' @param file.l1 file path for AMLR L1 nc file
#' @param file.l2 file path for AMLR L2 nc file
#' @param file.out.path file path to folder where output nc files will be created
#' @param glider.name character; name of glider
#' @param ... arguments passed to \code{\link{amlr_ngdac_nc_put}}
#'
#' @details This is the top-level function for converting AMLR glider data to NGDAC format v2.0,
#'   as described at the url below. The AMLR glider data is the output from
#'   \url{https://github.com/socib/glider_toolbox}, and thus consists of
#'   an L1 and L2 file containing time series (trajectory) and profile data, respectively.
#'   Data from both of these files is used to create the NGDAC-formatted nc files.
#'
#'   In addition to the variables time series variables listed at the NGDAC website,
#'   this function writes the following variables to the output nc files:
#'   TODO
#'
#' @return \code{TRUE} if the function ran successfully.
#'   This functions writes one nc file per profile to the folder specified by \code{file.out.path}
#'
#' @seealso \url{https://ioos.github.io/ioosngdac/ngdac-netcdf-file-format-version-2.html}
#'
#'   \url{http://cfconventions.org/Data/cf-standard-names/73/build/cf-standard-name-table.html}
#'
#' @examples
#'
#' \dontrun{
#' amlr_ngdac_convert(
#'   file.l1 = "AERD_18_19/dep100_amlr01_sgg3_L1_2018-12-06_data_rt.nc",
#'   file.l2 = "AERD_18_19/dep100_amlr01_sgg3_L2_2018-12-06_data_rt.nc",
#'   file.out.path = "Output/dep100_amlr01_sgg3/",
#'   glider.name = "amlr01"
#' )
#' }
#'
#' @export
amlr_ngdac_convert <- function(file.l1, file.l2, file.out.path, glider.name, ...) {
  stopifnot(
    inherits(file.l1, "character"),
    inherits(file.l2, "character"),
    inherits(file.out.path, "character"),
    inherits(glider.name, "character"),
    file.exists(file.l1),
    file.exists(file.l2),
    dir.exists(file.out.path)
  )

  if (!grepl("L1", file.l1))
    warning("The phrase 'L1' is not in the L1 file name. Provided L1 file name (file.l1):\n",
            file.l1, immediate. = TRUE)
  if (!grepl("L2", file.l2))
    warning("The phrase 'L2' is not in the L2 file name. Provided L2 file name (file.l2):\n",
            file.l2, immediate. = TRUE)

  if (!grepl("amlr", glider.name))
    warning("'amlr' is not in the glider name", immediate. = TRUE)


  #----------------------------------------------------------------------------
  ### Extract profile-level data from L2 file
  message("Extracting L2 data")
  x2 <- nc_open(file.l2)

  # TODO: add checks for formatting, etc. Hopefully there is some attribute to check as well

  x2.time <- as.POSIXct(ncvar_get(x2, "time"), origin = "1970-01-01")
  x2.lat  <- ncvar_get(x2, "latitude")
  x2.lon  <- ncvar_get(x2, "longitude")
  x2.prof <- ncvar_get(x2, "profile_index")

  nc_close(x2)

  profile.list <- lapply(seq_along(x2.prof), function(prof.idx) {
    list(
      time      = x2.time[prof.idx],
      latitude  = x2.lat[prof.idx],
      longitude = x2.lon[prof.idx],
      profile   = x2.prof[prof.idx]
    )
  })


  #----------------------------------------------------------------------------
  ### Extract time series data from L1 file
  message("Extracting L1 data")
  x1 <- nc_open(file.l1)

  x1.prof <- ncvar_get(x1, "profile_index")
  x1.time <- as.POSIXct(ncvar_get(x1, "time"), origin = "1970-01-01")
  x1.dep  <- ncvar_get(x1, "depth")
  x1.lat  <- ncvar_get(x1, "latitude")
  x1.lon  <- ncvar_get(x1, "longitude")
  x1.pres <- ncvar_get(x1, "pressure")
  x1.temp <- ncvar_get(x1, "temperature")
  x1.cond <- ncvar_get(x1, "conductivity")
  x1.sal  <- ncvar_get(x1, "salinity")
  x1.dens <- ncvar_get(x1, "density")
  x1.u    <- ncvar_get(x1, "water_velocity_eastward")
  x1.v    <- ncvar_get(x1, "water_velocity_northward")

  nc_close(x1)

  # Check that profile indices match between L1 and L2 files
  if (!(all(x2.prof %in% x1.prof) & all(x1.prof[x1.prof %% 1 == 0] %in% x2.prof)))
    stop("The profile indices of the L1 and L2 files do not match - ",
         "did you specify files from the same deployment?")

  # TODO: use parallel package here
  ts.list <- lapply(seq_along(x2.prof), function(prof.val) {
    prof.idx <- which(x1.prof == prof.val)

    list(
      time         = x1.time[prof.idx],
      depth        = x1.dep[prof.idx],
      latitude     = x1.lat[prof.idx],
      longitude    = x1.lon[prof.idx],
      pressure     = x1.pres[prof.idx],
      temperature  = x1.temp[prof.idx],
      conductivity = x1.cond[prof.idx],
      salinity     = x1.sal[prof.idx],
      density      = x1.dens[prof.idx],
      profile      = x1.prof[prof.idx],
      u            = x1.u[prof.idx],
      v            = x1.v[prof.idx]
    )
  })

  if (!all(sapply(ts.list, function(i) length(unique(sapply(i, length))) == 1)))
    stop("Error in extracting time series variables - unequal lengths")


  #----------------------------------------------------------------------------
  ### Generate one nc file per profile with data in NGDAC format (Glider DAC 3.0)

  # https://ioos.github.io/ioosngdac/ngdac-netcdf-file-format-version-2.html
  message("Generating nc files")
  for (i in x2.prof[1:5]) { #TODO: change
    # i <- x2.prof[1]

    # Get current data
    profile.curr <- profile.list[[i]]
    ts.curr <- ts.list[[i]]

    stopifnot(
      length(profile.curr$profile) == 1,
      profile.curr$profile == i,
      all(ts.curr$profile == i),
      profile.curr$time >= min(ts.curr$time),
      profile.curr$time <= max(ts.curr$time)
    )

    #------------------------------------------------------
    # Prep and file name
    dt.min <- min(ts.curr$time) #TODO: check that this is correct
    y.name <- paste0(
      file.out.path, glider.name, "-",
      format(dt.min, format = "%Y%m%d"), "T",
      format(dt.min, format = "%H%M%S"), "Z_delayed.nc"
    )


    #------------------------------------------------------
    # Dimensions
    time.unit <- "seconds since 1970-01-01T00:00:00Z"
    y.dim.time <- ncdim_def(
      "time", units = time.unit, vals = as.numeric(ts.curr$time), unlim = TRUE, calendar = "gregorian"
    )
    y.dim.traj <- ncdim_def(
      "traj_strlen", units = "", vals = seq_len(14+nchar(glider.name)), create_dimvar = FALSE
    )


    #------------------------------------------------------
    # Define variables - cleaner to do this as list so the list can just be passed to nc_create()
    vars.list <- list(
      #Data variables with dimensions
      ncvar_def("trajectory", units = "", dim = y.dim.traj, prec = "char"),
      ncvar_def("lat", units = "degrees_north", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("lon", units = "degrees_east", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("pressure", units = "dbar", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("depth", units = "m", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("temperature", units = "Celsius", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("conductivity", units = "S m-1", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("salinity", units = "PSU", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("density", units = "kg m-3", dim = y.dim.time, missval = -999, prec = "double"),

      #Dimensionless profile variables
      ncvar_def("profile_id", units = "", dim = list(), missval = -999, prec = "integer"),
      ncvar_def("profile_time", units = time.unit, dim = list(), missval = -999, prec = "double"),
      ncvar_def("profile_lat", units = "degrees_north", dim = list(), missval = -999, prec = "double"),
      ncvar_def("profile_lon", units = "degrees_east", dim = list(), missval = -999, prec = "double"),
      ncvar_def("time_uv", units = time.unit, dim = list(), missval = -999, prec = "double"),
      ncvar_def("lat_uv", units = "degrees_north", dim = list(), missval = -999, prec = "double"),
      ncvar_def("lon_uv", units = "degrees_east", dim = list(), missval = -999, prec = "double"),
      ncvar_def("u", units = "m s-1", dim = list(), missval = -999, prec = "double"),
      ncvar_def("v", units = "m s-1", dim = list(), missval = -999, prec = "double"),
      ncvar_def("platform", units = "", dim = list(), missval = -999, prec = "integer"),
      ncvar_def("instrument_ctd", units = "", dim = list(), missval = -999, prec = "integer"),

      #QC variables
      ncvar_def("time_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("lat_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("lon_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("pressure_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("depth_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("temperature_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("conductivity_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("salinity_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("density_qc", units = "", dim = y.dim.time, missval = -127, prec = "byte"),
      ncvar_def("profile_time_qc", units = "", dim = list(), missval = -127, prec = "byte"),
      ncvar_def("profile_lat_qc", units = "", dim = list(), missval = -127, prec = "byte"),
      ncvar_def("profile_lon_qc", units = "", dim = list(), missval = -127, prec = "byte"),
      ncvar_def("time_uv_qc", units = "", dim = list(), missval = -127, prec = "byte"),
      ncvar_def("lat_uv_qc", units = "", dim = list(), missval = -127, prec = "byte"),
      ncvar_def("lon_uv_qc", units = "", dim = list(), missval = -127, prec = "byte"),
      ncvar_def("u_qc", units = "", dim = list(), missval = -127, prec = "byte"),
      ncvar_def("v_qc", units = "", dim = list(), missval = -127, prec = "byte")
    )


    #------------------------------------------------------
    # Write file and add 1) variable data and attributes and 2) global attributes
    ncnew <- nc_create(y.name, vars = vars.list)
    # nc_close(ncnew)
    # amlr_ngdac_nc_put(ncnew.path = y.name, profile.curr, ts.curr, glider.name, ...)

    # tryCatch({
    amlr_ngdac_nc_put(ncnew = ncnew, profile.curr, ts.curr, glider.name, ...)
    # }, error = nc_close(ncnew))
    # ncnew
    nc_close(ncnew)
  }

  TRUE
}
