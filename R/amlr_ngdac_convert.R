#' Convert
#'
#' Convert AMLR glider data to NGDAC format
#'
#' @param file.l1 file path for AMLR L1 nc file
#' @param file.l2 file path for AMLR L2 nc file
#' @param path.out file path to folder where output nc files will be created
#' @param glider.name character; name of glider
#'
#' @details
#'
#' @return
#'
#' @seealso
#'
#' @examples
#'
#' @export
amlr_ngdac_convert <- function(file.l1, file.l2, file.out.path, glider.name) {

  stopifnot(require(ncdf4), require(dplyr))

  #----------------------------------------------------------------------------
  ### Extract profile-level data from L2 file
  message("Extracting L2 data")
  x2 <- nc_open(file.l2)

  # TODO: add checks for formatting, etc. Hopefully there is some attribute to check as well

  # sink("dep100_amlr01_sgg3_L2_2018-12-06_data_rt.txt")
  # print(x2)
  # sink()
  #
  # names(x2$var)
  # names(x2$dim)

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
    y.dim.time <- ncdim_def(
      "time", units = "seconds since 1970-01-01T00:00:00Z", vals = as.numeric(ts.curr$time),
      unlim = TRUE, calendar = "gregorian", longname = "Time"
    )
    y.dim.traj <- ncdim_def(
      "traj_strlen", units = "", vals = seq_len(14+nchar(glider.name)),
      create_dimvar = FALSE
    )


    #------------------------------------------------------
    # Define variables - cleaner to do this as list so the list can just be passed to nc_create()
    vars.list <- list(
      #Data variables with dimensions
      ncvar_def("trajectory", units = "", dim = y.dim.traj, prec = "char", longname = "Trajectory/Deployment Name"),
      ncvar_def("lat", units = "degrees_north", dim = y.dim.time, missval = -999, longname = "Latitude", prec = "double"),
      ncvar_def("lon", units = "degrees_east", dim = y.dim.time, missval = -999, longname = "Longitude", prec = "double"),
      ncvar_def("pressure", units = "dbar", dim = y.dim.time, missval = -999, longname = "Pressure", prec = "double"),
      ncvar_def("depth", units = "m", dim = y.dim.time, missval = -999, longname = "Depth", prec = "double"),
      ncvar_def("temperature", units = "Celsius", dim = y.dim.time, missval = -999, longname = "Temperature", prec = "double"),
      ncvar_def("conductivity", units = "S m-1", dim = y.dim.time, missval = -999, longname = "Conductivity", prec = "double"),
      ncvar_def("salinity", units = "PSU", dim = y.dim.time, missval = -999, longname = "Salinity", prec = "double"),
      ncvar_def("density", units = "kg m-3", dim = y.dim.time, missval = -999, longname = "Density", prec = "double"),

      #Dimensionless profile variables
      ncvar_def("profile_id", units = "", dim = list(), missval = -999, longname = "Profile ID", prec = "integer"),
      ncvar_def("profile_time", units = "seconds since 1970-01-01T00:00:00Z", dim = list(), missval = -999,
                longname = "Profile Center Time", prec = "double"),
      ncvar_def("profile_lat", units = "degrees_north", dim = list(), missval = -999, longname = "Profile Center Latitude", prec = "double"),
      ncvar_def("profile_lon", units = "degrees_east", dim = list(), missval = -999, longname = "Profile Center Longitude", prec = "double"),
      ncvar_def("time_uv", units = "seconds since 1970-01-01T00:00:00Z", dim = list(), missval = -999,
                longname = "Depth-Averaged Time", prec = "double"),
      ncvar_def("lat_uv", units = "degrees_north", dim = list(), missval = -999, longname = "Depth-Averaged Latitude", prec = "double"),
      ncvar_def("lon_uv", units = "degrees_east", dim = list(), missval = -999, longname = "Depth-Averaged Longitude", prec = "double"),
      ncvar_def("u", units = "m s-1", dim = list(), missval = -999, longname = "Depth-Averaged Eastward Sea Water Velocity", prec = "double"),
      ncvar_def("v", units = "m s-1", dim = list(), missval = -999, longname = "Depth-Averaged Northward Sea Water Velocity", prec = "double"),
      ncvar_def("platform", units = "", dim = list(), missval = -999, prec = "integer"),
      ncvar_def("instrument_ctd", units = "", dim = list(), missval = -999, prec = "integer"),

      #QC variables
      ncvar_def("time_qc", units = "", dim = y.dim.time, missval = -127, longname = "time Quality Flag", prec = "byte"),
      ncvar_def("lat_qc", units = "", dim = y.dim.time, missval = -127, longname = "lat Quality Flag", prec = "byte"),
      ncvar_def("lon_qc", units = "", dim = y.dim.time, missval = -127, longname = "lon Quality Flag", prec = "byte"),
      ncvar_def("pressure_qc", units = "", dim = y.dim.time, missval = -127, longname = "pressure Quality Flag", prec = "byte"),
      ncvar_def("depth_qc", units = "", dim = y.dim.time, missval = -127, longname = "depth Quality Flag", prec = "byte"),
      ncvar_def("temperature_qc", units = "", dim = y.dim.time, missval = -127, longname = "temperature Quality Flag", prec = "byte"),
      ncvar_def("conductivity_qc", units = "", dim = y.dim.time, missval = -127, longname = "conductivity Quality Flag", prec = "byte"),
      ncvar_def("salinity_qc", units = "", dim = y.dim.time, missval = -127, longname = "salinity Quality Flag", prec = "byte"),
      ncvar_def("density_qc", units = "", dim = y.dim.time, missval = -127, longname = "density Quality Flag", prec = "byte"),
      ncvar_def("profile_time_qc", units = "", dim = list(), missval = -127, longname = "profile_time Quality Flag", prec = "byte"),
      ncvar_def("profile_lat_qc", units = "", dim = list(), missval = -127, longname = "profile_lat Quality Flag", prec = "byte"),
      ncvar_def("profile_lon_qc", units = "", dim = list(), missval = -127, longname = "profile_lon Quality Flag", prec = "byte"),
      ncvar_def("time_uv_qc", units = "", dim = list(), missval = -127, longname = "time_uv Quality Flag", prec = "byte"),
      ncvar_def("lat_uv_qc", units = "", dim = list(), missval = -127, longname = "lat_uv Quality Flag", prec = "byte"),
      ncvar_def("lon_uv_qc", units = "", dim = list(), missval = -127, longname = "lon_uv Quality Flag", prec = "byte"),
      ncvar_def("u_qc", units = "", dim = list(), missval = -127, longname = "u Quality Flag", prec = "byte"),
      ncvar_def("v_qc", units = "", dim = list(), missval = -127, longname = "v Quality Flag", prec = "byte")
    )


    #------------------------------------------------------
    # Write file and add 1) variable data and attributes and 2) global attributes
    # browser()

    ncnew <- nc_create(y.name, vars = vars.list)
    ngdac_nc_put(ncnew, profile.curr, ts.curr, glider.name)
    ncnew
    nc_close(ncnew)
  }
}
