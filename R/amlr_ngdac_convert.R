#' Convert AMLR L1 and L2 glider data to NGDAC format v2.0
#'
#' Convert AMLR L1 and L2 glider data to NGDAC format v2.0
#'
#' @param file.l1 file path for AMLR L1 nc file
#' @param file.l2 file path for AMLR L2 nc file
#' @param file.out.path file path to folder where output nc files will be created
#' @param glider.name character; name of glider
#' @param wmo.id character; the WMO ID of the glider/deployment.
#'   If there is no WMO ID, this should be a single space, i.e. \code{" "}
#' @param ctd.info list; CTD information. Must contain the following named elements:
#'   calib_date (Date), calib_date_factory (Date), and serial_num (character)
#' @param flbbcd.info list; flbbcd information. Must contain the following named elements:
#'   calib_date (Date), calib_date_factory (Date), and serial_num (character)
#' @param oxygen.info list; oxygen oxygen information. Must contain the following named elements:
#'   calib_date (Date), calib_date_factory (Date), and serial_num (character)
#' @param ctd.comment character; string to pass to 'comment' attribute of 'instrument_ctd' variable.
#'   Default is \code{"pumped CTD"}
#' @param flbbcd.comment character; string to pass to 'comment' attribute of 'instrument_flbbcd' variable.
#'   Default is \code{" "}
#' @param oxygen.comment character; string to pass to 'comment' attribute of 'instrument_oxygen' variable.
#'   Default is \code{" "}
#'
#' @details Raw AMLR glider data is initially processed using the
#'   SOCIB glider toolbox (\url{https://github.com/socib/glider_toolbox}).
#'   Thus, processed AMLR glider data consists of an L1 and L2 file containing
#'   time series (trajectory) and profile data, respectively.
#'   This function takes these L1 and L2 output files and converts them to NGDAC format v2.0.
#'   This process consists of creating individual nc files for each profile,
#'   and extracting both time series and profile-level information for each new nc file.
#'   See the NGDAC wiki \url{https://ioos.github.io/ioosngdac/ngdac-netcdf-file-format-version-2.html}
#'   for more details.
#'
#'   In addition to the variables time series variables listed at the NGDAC website,
#'   this function writes several other variables to the output nc files.
#'   These data are collected using flbbcd and oxygen instruments; the metadata for
#'   these instruments is included in instrument_flbbcd and instrument_oxygen, respectively.
#'   The variables, along with their standard names (from
#'   \url{http://cfconventions.org/Data/cf-standard-names/73/build/cf-standard-name-table.html})
#'   are as follows:
#'
#'   \tabular{l}{
#'     cdom: concentration_of_colored_dissolved_organic_matter_in_sea_water_expressed_as_equivalent_mass_fraction_of_quinine_sulfate_dihydrate\cr
#'     chlorophyll: concentration_of_chlorophyll_in_sea_water\cr
#'     backscatter: volume_backwards_scattering_coefficient_of_radiative_flux_in_sea_water (from backscatter_700 variable)\cr
#'     radiation_wavelength: radiation_wavelength (700 for all values)\cr
#'     oxygen_saturation: fractional_saturation_of_oxygen_in_sea_water\cr
#'     oxygen_concentration: mole_concentration_of_dissolved_molecular_oxygen_in_sea_water
#'     }
#'
#'   When adding data and attributes to the new nc file(s),
#'   data is extracted from the L1 and L2 files where applicable (e.g. most variable standard names),
#'   but most of the codes/names/info are hard-coded in the function and thus the function will
#'   need to be changed if 1) the L1/L2 file structure changes or 2) the NGDAC requirements change.
#'   Dynamic (aka deployment-specific) information is either calculated from the data
#'   (e.g. deployment date/time) or passed to function via arguments (e.g. CTD calibration information)
#'
#' @return \code{TRUE} if the function ran successfully.
#'   This functions writes one nc file per profile to the folder specified by \code{file.out.path}
#'
#' @examples
#' \dontrun{
#' amlr_ngdac_convert( #This is not full deployment data
#'   file.l1 = "AERD_18_19/dep100_amlr01_sgg3_L1_2018-12-06_data_rt.nc",
#'   file.l2 = "AERD_18_19/dep100_amlr01_sgg3_L2_2018-12-06_data_rt.nc",
#'   file.out.path = "Output/dep100_amlr01_sgg3/",
#'   glider.name = "amlr01", wmo.id = "0",
#'   ctd.info = list(
#'     calib_date = as.Date("2000-01-01"), calib_date_factory = as.Date("2000-01-01"),
#'     serial_num = "1"
#'   ),
#'   flbbcd.info = list(
#'     calib_date = as.Date("2000-01-01"), calib_date_factory = as.Date("2000-01-01"),
#'     serial_num = "3"
#'   ),
#'   optode.info = list(
#'     calib_date = as.Date("2000-01-01"), calib_date_factory = as.Date("2000-01-01"),
#'     serial_num = "2"
#'   )
#' )
#' }
#'
#' @export
amlr_ngdac_convert <- function(file.l1, file.l2, file.out.path, glider.name,
                               wmo.id, ctd.info, flbbcd.info, oxygen.info,
                               ctd.comment = "pumped CTD", flbbcd.comment = " ",
                               oxygen.comment = " ") {
  #----------------------------------------------------------------------------
  stopifnot(
    inherits(file.l1, "character"),
    inherits(file.l2, "character"),
    inherits(file.out.path, "character"),
    inherits(glider.name, "character"),
    file.exists(file.l1),
    file.exists(file.l2),
    dir.exists(file.out.path),
    inherits(glider.name, "character"),
    inherits(wmo.id, "character"),

    inherits(ctd.info, "list"),
    inherits(oxygen.info, "list"),
    inherits(flbbcd.info, "list"),
    identical(names(ctd.info), c("calib_date", "calib_date_factory", "serial_num")),
    identical(names(oxygen.info), c("calib_date", "calib_date_factory", "serial_num")),
    identical(names(flbbcd.info), c("calib_date", "calib_date_factory", "serial_num")),

    inherits(ctd.info$calib_date, "Date"), inherits(oxygen.info$calib_date, "Date"),
    inherits(flbbcd.info$calib_date, "Date"),
    inherits(ctd.info$calib_date_factory, "Date"), inherits(oxygen.info$calib_date_factory, "Date"),
    inherits(flbbcd.info$calib_date_factory, "Date"),
    inherits(ctd.info$serial_num, "character"), inherits(oxygen.info$serial_num, "character"),
    inherits(flbbcd.info$serial_num, "character"),

    inherits(ctd.comment, "character"),
    inherits(oxygen.comment, "character"),
    inherits(flbbcd.comment, "character")
  )

  if (!grepl("L1", file.l1))
    warning("The phrase 'L1' is not in the L1 file name; is this actually the L1 file?. ",
            "Provided L1 file name (file.l1):\n",
            file.l1, immediate. = TRUE)
  if (!grepl("L2", file.l2))
    warning("The phrase 'L2' is not in the L2 file name; is this actually the L1 file?. ",
            "Provided L2 file name (file.l2):\n",
            file.l2, immediate. = TRUE)

  if (!grepl("amlr", glider.name)) stop("'amlr' is not in the glider name")
  if (!(grepl(glider.name, file.l1) & grepl(glider.name, file.l2)))
    warning("At least one of the output file path or the L1/L2 file names does not include ",
            "the user provided glider name: ", glider.name, "\n",
            "Did you provide the correct file paths and glider name?")


  #----------------------------------------------------------------------------
  ### Extract profile-level data from L2 file
  message("Extracting L2 data")
  x2 <- nc_open(file.l2)

  x2.df <- data.frame(
    profile   = ncvar_get(x2, "profile_index"),
    time      = as.POSIXct(ncvar_get(x2, "time"), origin = "1970-01-01"),
    latitude  = ncvar_get(x2, "latitude"),
    longitude = ncvar_get(x2, "longitude"),
    stringsAsFactors = FALSE
  )
  stopifnot(length(unique(x2.df$profile)) == nrow(x2.df))


  #----------------------------------------------------------------------------
  ### Extract time series data from L1 file
  message("Extracting L1 data")
  x1 <- nc_open(file.l1)

  x1.df <- data.frame(
    profile = ncvar_get(x1, "profile_index"),
    time = as.POSIXct(ncvar_get(x1, "time"), origin = "1970-01-01"),
    depth = ncvar_get(x1, "depth"),
    latitude = ncvar_get(x1, "latitude"),
    longitude = ncvar_get(x1, "longitude"),
    pressure = ncvar_get(x1, "pressure"),
    temperature = ncvar_get(x1, "temperature"),
    conductivity = ncvar_get(x1, "conductivity"),
    salinity = ncvar_get(x1, "salinity"),
    density = ncvar_get(x1, "density"),
    u = ncvar_get(x1, "water_velocity_eastward"),
    v = ncvar_get(x1, "water_velocity_northward"),
    oxygen_saturation = ncvar_get(x1, "oxygen_saturation"),
    oxygen_concentration = ncvar_get(x1, "oxygen_concentration"),
    cdom = ncvar_get(x1, "cdom"),
    chlorophyll = ncvar_get(x1, "chlorophyll"),
    backscatter_700 = ncvar_get(x1, "backscatter_700"),
    stringsAsFactors = FALSE
  )


  # Check that profile indices match between L1 and L2 files
  if (!all(x2.df$profile %in% x1.df$profile)) {
    # Nuance - sometimes there is no L1 data for a profile index. In these cases,
    #   the L2 data has a profile index but all NA data
    profile.nox1 <- setdiff(x2.df$profile, x1.df$profile)
    stopifnot(identical(names(x2.df)[1], "profile"))
    profile.nox1.vals <- c(
      lapply(x2.df[, -1], function(i) {
        as.character(i[profile.nox1]) #Needed for time values
      }),
      lapply(setdiff(names(x2$var), c("profile_index", "longitude", "latitude")), function(i) {
        ncvar_get(x2, i)[, profile.nox1]
      }),
      list(time = as.character(as.POSIXct(ncvar_get(x2, "time"), origin = "1970-01-01"))[profile.nox1])
    )
    # names(profile.nox1.vals) <- c( #For debugging
    #   names(x2.df[, -1]), setdiff(names(x2$var), c("profile_index", "longitude", "latitude")), "time"
    # )

    # Check that all L2 variable values are NA for these indices
    if (!all(vapply(profile.nox1.vals, function(j) all(is.na(j) | j == 0), as.logical(1)))) {
      stop("The profile indices in the L2 file but not the L1 file have non-Na time/lat/lon values - ",
           "did you specify files from the same deployment?")
    } else {
      x2.df <- x2.df %>% filter(.data$profile %in% x1.df$profile)
    }


  } else if (!all(x1.df$profile[x1.df$profile %% 1 == 0] %in% x2.df$profile)) {
    stop("The L1 file contains profile indices that are not in the L2 file - ",
         "did you specify files from the same deployment?")
  }


  ### Sanity checks
  if (!all(x2.df$profile %in% x1.df$profile) ||
      !all(x1.df$profile[x1.df$profile %% 1 == 0] %in% x2.df$profile))
    stop("The profile indices of the L1 and L2 files do not match - ",
         "did you specify files from the same deployment?")

  if (anyNA(x2.df) | anyNA(x1.df[, c("profile", "time")]))
    stop("Profile, time, or profile-level lat/lon values are NA")


  #----------------------------------------------------------------------------
  ### Generate one nc file per profile with data in NGDAC format (Glider DAC 3.0)
  # Parallel-izing wasn't worth the 'effort'


  # https://ioos.github.io/ioosngdac/ngdac-netcdf-file-format-version-2.html
  message("Generating ", nrow(x2.df), " nc files")
  for (i in x2.df$profile) {
    # Get current data
    profile.curr <- x2.df %>% filter(.data$profile == i)
    ts.curr <- x1.df %>% filter(.data$profile == i)

    stopifnot(
      nrow(profile.curr) == 1,
      between(profile.curr$time, min(ts.curr$time), max(ts.curr$time))
    )

    #----------------------------------------------------------------
    # Define variables and dimensions -
    #   cleaner to do this as list so the list can just be passed to nc_create()
    #   long_name is set in later for consistency
    time.unit <- "seconds since 1970-01-01T00:00:00Z"
    y.dim.time <- ncdim_def(
      "time", units = time.unit, vals = as.numeric(ts.curr$time), unlim = TRUE, calendar = "gregorian"
    )
    y.dim.traj <- ncdim_def(
      "traj_strlen", units = "", vals = seq_len(14+nchar(glider.name)), create_dimvar = FALSE
    )


    vars.list <- list(
      #Data variables with dimensions
      ncvar_def("trajectory", units = "", dim = y.dim.traj, prec = "char"),
      ncvar_def("lat", units = ncatt_get(x1, "latitude")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("lon", units = ncatt_get(x1, "longitude")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("pressure", units = ncatt_get(x1, "pressure")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("depth", units = ncatt_get(x1, "depth")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("temperature", units = ncatt_get(x1, "temperature")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("conductivity", units = ncatt_get(x1, "conductivity")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("salinity", units = "1", dim = y.dim.time, missval = -999, prec = "double"), #L1 units are 'PSU'
      ncvar_def("density", units = ncatt_get(x1, "density")$units, dim = y.dim.time, missval = -999, prec = "double"),

      ncvar_def("oxygen_saturation", units = "percent", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("oxygen_concentration", units = ncatt_get(x1, "oxygen_concentration")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("cdom", units = ncatt_get(x1, "cdom")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("chlorophyll", units = ncatt_get(x1, "chlorophyll")$units, dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("radiation_wavelength", units = "nm", dim = y.dim.time, missval = -999, prec = "double"),
      ncvar_def("backscatter", units = "m-1", dim = y.dim.time, missval = -999, prec = "double"),

      #Dimensionless profile variables
      ncvar_def("profile_id", units = "", dim = list(), missval = -999, prec = "integer"),
      ncvar_def("profile_time", units = time.unit, dim = list(), missval = -999, prec = "double"),
      ncvar_def("profile_lat", ncatt_get(x2, "latitude")$units, dim = list(), missval = -999, prec = "double"),
      ncvar_def("profile_lon", ncatt_get(x2, "longitude")$units, dim = list(), missval = -999, prec = "double"),
      ncvar_def("time_uv", units = time.unit, dim = list(), missval = -999, prec = "double"),
      ncvar_def("lat_uv", units = ncatt_get(x1, "latitude")$units, dim = list(), missval = -999, prec = "double"),
      ncvar_def("lon_uv", units = ncatt_get(x1, "longitude")$units, dim = list(), missval = -999, prec = "double"),
      ncvar_def("u", units = ncatt_get(x1, "water_velocity_eastward")$units, dim = list(), missval = -999, prec = "double"),
      ncvar_def("v", units = ncatt_get(x1, "water_velocity_northward")$units, dim = list(), missval = -999, prec = "double"),

      ncvar_def("platform", units = "", dim = list(), missval = -999, prec = "integer"),
      ncvar_def("instrument_ctd", units = "", dim = list(), missval = -999, prec = "integer"),
      ncvar_def("instrument_oxygen", units = "", dim = list(), missval = -999, prec = "integer"),
      ncvar_def("instrument_flbbcd", units = "", dim = list(), missval = -999, prec = "integer"),

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


    #----------------------------------------------------------------
    # Write file and add 1) variable data and attributes and 2) global attributes
    y.name <- paste0(
      glider.name, "-", format(min(ts.curr$time), format = "%Y%m%dT%H%M%S"),
      "Z_delayed.nc"
    )
    ncnew <- nc_create(paste0(file.out.path, y.name), vars = vars.list)

    tryCatch({
      #----------------------------------------------------
      ### Add data to variables
      y.traj <- paste0(
        glider.name, "-", format(min(ts.curr$time, na.rm = TRUE), format = "%Y%m%dT%H%M")
      )

      # Time series variables
      ts.count <- length(ts.curr$time)
      qc.val <- rep(as.integer(0), ts.count)

      ncvar_put(ncnew, "time", ts.curr$time, start = 1, count = ts.count)
      ncvar_put(ncnew, "trajectory", y.traj)
      ncvar_put(ncnew, "lat", ts.curr$latitude, start = 1, count = ts.count)
      ncvar_put(ncnew, "lon", ts.curr$longitude, start = 1, count = ts.count)
      ncvar_put(ncnew, "pressure", ts.curr$pressure, start = 1, count = ts.count)
      ncvar_put(ncnew, "depth", ts.curr$depth, start = 1, count = ts.count)
      ncvar_put(ncnew, "temperature", ts.curr$temperature, start = 1, count = ts.count)
      ncvar_put(ncnew, "conductivity", ts.curr$conductivity, start = 1, count = ts.count)
      ncvar_put(ncnew, "salinity", ts.curr$salinity, start = 1, count = ts.count)
      ncvar_put(ncnew, "density", ts.curr$density, start = 1, count = ts.count)

      ncvar_put(ncnew, "oxygen_saturation", ts.curr$oxygen_saturation, start = 1, count = ts.count)
      ncvar_put(ncnew, "oxygen_concentration", ts.curr$oxygen_concentration, start = 1, count = ts.count)
      ncvar_put(ncnew, "cdom", ts.curr$cdom, start = 1, count = ts.count)
      ncvar_put(ncnew, "chlorophyll", ts.curr$chlorophyll, start = 1, count = ts.count)
      ncvar_put(ncnew, "radiation_wavelength", rep(700, ts.count), start = 1, count = ts.count)
      ncvar_put(ncnew, "backscatter", ts.curr$backscatter_700, start = 1, count = ts.count)

      ncvar_put(ncnew, "time_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "lat_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "lon_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "pressure_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "depth_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "temperature_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "conductivity_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "salinity_qc", qc.val, start = 1, count = ts.count)
      ncvar_put(ncnew, "density_qc", qc.val, start = 1, count = ts.count)

      # Profile variables (dimensionless)
      ncvar_put(ncnew, "profile_id", profile.curr$profile)
      ncvar_put(ncnew, "profile_time", profile.curr$time)
      ncvar_put(ncnew, "profile_lat", profile.curr$latitude)
      ncvar_put(ncnew, "profile_lon", profile.curr$longitude)
      ncvar_put(ncnew, "time_uv", NA)
      ncvar_put(ncnew, "lat_uv", NA)
      ncvar_put(ncnew, "lon_uv", NA)
      ncvar_put(ncnew, "u", NA)
      ncvar_put(ncnew, "v", NA)

      ncvar_put(ncnew, "profile_time_qc", 0)
      ncvar_put(ncnew, "profile_lat_qc", 0)
      ncvar_put(ncnew, "profile_lon_qc", 0)
      ncvar_put(ncnew, "time_uv_qc", 0)
      ncvar_put(ncnew, "lat_uv_qc", 0)
      ncvar_put(ncnew, "lon_uv_qc", 0)
      ncvar_put(ncnew, "u_qc", 0)
      ncvar_put(ncnew, "v_qc", 0)

      # 'platform' and 'instrument_' vars have no values, just attributes



      #----------------------------------------------------
      ### Add attributes to variables

      ncatt_put(ncnew, "time", "ancillary_variables", "time_qc")
      ncatt_put(ncnew, "time", "comment", "Measured or calculated time at each point in the time-series")
      ncatt_put(ncnew, "time", "long_name", "Time")
      ncatt_put(ncnew, "time", "observation_type", "measured")
      ncatt_put(ncnew, "time", "standard_name", ncatt_get(x1, "time")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "time_qc", "time", ncatt_get(x1, "time")$standard_name)

      ncatt_put(ncnew, "trajectory", "cf_role", "trajectory_id")
      ncatt_put(ncnew, "trajectory", "comment", "A trajectory is a single deployment of a glider and may span multiple data files.")
      ncatt_put(ncnew, "trajectory", "long_name", "Trajectory/Deployment Name")

      ncatt_put(ncnew, "lat", "ancillary_variables", "lat_qc")
      ncatt_put(ncnew, "lat", "comment", "Values may be interpolated between measured GPS fixes")
      ncatt_put(ncnew, "lat", "coordinate_reference_frame", "urn:ogc:crs:EPSG::4326")
      ncatt_put(ncnew, "lat", "long_name", "Latitude")
      ncatt_put(ncnew, "lat", "observation_type", "measured")
      ncatt_put(ncnew, "lat", "platform", "platform")
      ncatt_put(ncnew, "lat", "reference", "WGS84")
      ncatt_put(ncnew, "lat", "standard_name", ncatt_get(x1, "latitude")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "lat_qc", "latitude", ncatt_get(x1, "latitude")$standard_name)
      valid_put_check(ncnew, ts.curr$latitude, -90, 90, "lat", y.traj)

      ncatt_put(ncnew, "lon", "ancillary_variables", "lon_qc")
      ncatt_put(ncnew, "lon", "comment", "Values may be interpolated between measured GPS fixes")
      ncatt_put(ncnew, "lon", "coordinate_reference_frame", "urn:ogc:crs:EPSG::4326")
      ncatt_put(ncnew, "lon", "long_name", "Longitude")
      ncatt_put(ncnew, "lon", "observation_type", "measured")
      ncatt_put(ncnew, "lon", "platform", "platform")
      ncatt_put(ncnew, "lon", "reference", "WGS84")
      ncatt_put(ncnew, "lon", "standard_name", ncatt_get(x1, "longitude")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "lon_qc", "longitude", ncatt_get(x1, "longitude")$standard_name)
      valid_put_check(ncnew, ts.curr$longitude, -180, 180, "lon", y.traj)

      ncatt_put(ncnew, "pressure", "accuracy", " ")
      ncatt_put(ncnew, "pressure", "ancillary_variables", "pressure_qc")
      ncatt_put(ncnew, "pressure", "comment", " ")
      ncatt_put(ncnew, "pressure", "instrument", "instrument_ctd")
      ncatt_put(ncnew, "pressure", "long_name", "Pressure")
      ncatt_put(ncnew, "pressure", "observation_type", "measured")
      ncatt_put(ncnew, "pressure", "platform", "platform")
      ncatt_put(ncnew, "pressure", "positive", "down")
      ncatt_put(ncnew, "pressure", "precision", " ")
      ncatt_put(ncnew, "pressure", "reference_datum", "sea-surface")
      ncatt_put(ncnew, "pressure", "resolution", " ")
      ncatt_put(ncnew, "pressure", "standard_name", "sea_water_pressure") #ncatt_get(x1, "pressure")$standard_name
      amlr_ngdac_nc_put_qc(ncnew, "pressure_qc", "pressure", "sea_water_pressure")
      valid_put_check(ncnew, ts.curr$pressure, 0, 2000, "pressure", y.traj)

      ncatt_put(ncnew, "depth", "accuracy", " ")
      ncatt_put(ncnew, "depth", "ancillary_variables", "depth_qc")
      ncatt_put(ncnew, "depth", "comment", " ")
      ncatt_put(ncnew, "depth", "instrument", "instrument_ctd")
      ncatt_put(ncnew, "depth", "long_name", "Depth")
      ncatt_put(ncnew, "depth", "observation_type", "calculated")
      ncatt_put(ncnew, "depth", "platform", "platform")
      ncatt_put(ncnew, "depth", "positive", "down")
      ncatt_put(ncnew, "depth", "precision", " ")
      ncatt_put(ncnew, "depth", "reference_datum", "sea-surface")
      ncatt_put(ncnew, "depth", "resolution", " ")
      ncatt_put(ncnew, "depth", "standard_name", ncatt_get(x1, "depth")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "depth_qc", "depth", ncatt_get(x1, "depth")$standard_name)
      valid_put_check(ncnew, ts.curr$depth, 0, 2000, "depth", y.traj)

      ncatt_put(ncnew, "temperature", "accuracy", " ")
      ncatt_put(ncnew, "temperature", "ancillary_variables", "temperature_qc")
      ncatt_put(ncnew, "temperature", "instrument", "instrument_ctd")
      ncatt_put(ncnew, "temperature", "long_name", "Temperature")
      ncatt_put(ncnew, "temperature", "observation_type", "measured")
      ncatt_put(ncnew, "temperature", "platform", "platform")
      ncatt_put(ncnew, "temperature", "precision", " ")
      ncatt_put(ncnew, "temperature", "resolution", " ")
      ncatt_put(ncnew, "temperature", "standard_name", ncatt_get(x1, "temperature")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "temperature_qc", "temperature", ncatt_get(x1, "temperature")$standard_name)
      valid_put_check(ncnew, ts.curr$temperature, -5, 40, "temperature", y.traj)

      ncatt_put(ncnew, "conductivity", "accuracy", " ")
      ncatt_put(ncnew, "conductivity", "ancillary_variables", "conductivity_qc")
      ncatt_put(ncnew, "conductivity", "instrument", "instrument_ctd")
      ncatt_put(ncnew, "conductivity", "long_name", "Conductivity")
      ncatt_put(ncnew, "conductivity", "observation_type", "measured")
      ncatt_put(ncnew, "conductivity", "platform", "platform")
      ncatt_put(ncnew, "conductivity", "precision", " ")
      ncatt_put(ncnew, "conductivity", "resolution", " ")
      ncatt_put(ncnew, "conductivity", "standard_name", "sea_water_electrical_conductivity") #ncatt_get(x1, "conductivity")$standard_name
      amlr_ngdac_nc_put_qc(ncnew, "conductivity_qc", "conductivity", "sea_water_electrical_conductivity")
      valid_put_check(ncnew, ts.curr$conductivity, 0, 10, "conductivity", y.traj)

      ncatt_put(ncnew, "salinity", "accuracy", " ")
      ncatt_put(ncnew, "salinity", "ancillary_variables", "salinity_qc")
      ncatt_put(ncnew, "salinity", "instrument", "instrument_ctd")
      ncatt_put(ncnew, "salinity", "long_name", "Salinity")
      ncatt_put(ncnew, "salinity", "observation_type", "calculated")
      ncatt_put(ncnew, "salinity", "platform", "platform")
      ncatt_put(ncnew, "salinity", "precision", " ")
      ncatt_put(ncnew, "salinity", "resolution", " ")
      ncatt_put(ncnew, "salinity", "standard_name", "sea_water_practical_salinity") #ncatt_get(x1, "salinity")$standard_name
      amlr_ngdac_nc_put_qc(ncnew, "salinity_qc", "salinity", "sea_water_practical_salinity")
      valid_put_check(ncnew, ts.curr$salinity, 0, 40.0, "salinity", y.traj)

      ncatt_put(ncnew, "density", "accuracy", " ")
      ncatt_put(ncnew, "density", "ancillary_variables", "density_qc")
      ncatt_put(ncnew, "density", "instrument", "instrument_ctd")
      ncatt_put(ncnew, "density", "long_name", "Density")
      ncatt_put(ncnew, "density", "observation_type", "calculated")
      ncatt_put(ncnew, "density", "platform", "platform")
      ncatt_put(ncnew, "density", "precision", " ")
      ncatt_put(ncnew, "density", "resolution", " ")
      ncatt_put(ncnew, "density", "standard_name", ncatt_get(x1, "density")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "density_qc", "density", ncatt_get(x1, "density")$standard_name)
      valid_put_check(ncnew, ts.curr$density, 1015.0, 1040.0, "density", y.traj)

      ncatt_put(ncnew, "oxygen_saturation", "accuracy", " ")
      ncatt_put(ncnew, "oxygen_saturation", "instrument", "instrument_oxygen")
      ncatt_put(ncnew, "oxygen_saturation", "long_name", "Oxygen Saturation")
      ncatt_put(ncnew, "oxygen_saturation", "observation_type", "measured")
      ncatt_put(ncnew, "oxygen_saturation", "platform", "platform")
      ncatt_put(ncnew, "oxygen_saturation", "precision", " ")
      ncatt_put(ncnew, "oxygen_saturation", "resolution", " ")
      ncatt_put(ncnew, "oxygen_saturation", "standard_name", ncatt_get(x1, "oxygen_saturation")$standard_name)
      valid_put_check(ncnew, ts.curr$oxygen_saturation, 0, 120, "oxygen_saturation", y.traj)

      ncatt_put(ncnew, "oxygen_concentration", "accuracy", " ")
      ncatt_put(ncnew, "oxygen_concentration", "instrument", "instrument_oxygen")
      ncatt_put(ncnew, "oxygen_concentration", "long_name", "Oxygen Concentration")
      ncatt_put(ncnew, "oxygen_concentration", "observation_type", "calculated")
      ncatt_put(ncnew, "oxygen_concentration", "platform", "platform")
      ncatt_put(ncnew, "oxygen_concentration", "precision", " ")
      ncatt_put(ncnew, "oxygen_concentration", "resolution", " ")
      ncatt_put(ncnew, "oxygen_concentration", "standard_name", ncatt_get(x1, "oxygen_concentration")$standard_name)
      valid_put_check(ncnew, ts.curr$oxygen_concentration, 0, 500, "oxygen_concentration", y.traj)

      ncatt_put(ncnew, "cdom", "accuracy", " ")
      ncatt_put(ncnew, "cdom", "instrument", "instrument_flbbcd")
      ncatt_put(ncnew, "cdom", "long_name", "CDOM")
      ncatt_put(ncnew, "cdom", "observation_type", "measured")
      ncatt_put(ncnew, "cdom", "platform", "platform")
      ncatt_put(ncnew, "cdom", "precision", " ")
      ncatt_put(ncnew, "cdom", "resolution", " ")
      ncatt_put(ncnew, "cdom", "standard_name", #ncatt_get(x1, "cdom")$standard_name
                "concentration_of_colored_dissolved_organic_matter_in_sea_water_expressed_as_equivalent_mass_fraction_of_quinine_sulfate_dihydrate")
      valid_put_check(ncnew, ts.curr$cdom, 0, 375, "cdom", y.traj)

      ncatt_put(ncnew, "chlorophyll", "accuracy", " ")
      ncatt_put(ncnew, "chlorophyll", "instrument", "instrument_flbbcd")
      ncatt_put(ncnew, "chlorophyll", "long_name", "Chlorophyll")
      ncatt_put(ncnew, "chlorophyll", "observation_type", "measured")
      ncatt_put(ncnew, "chlorophyll", "platform", "platform")
      ncatt_put(ncnew, "chlorophyll", "precision", " ")
      ncatt_put(ncnew, "chlorophyll", "resolution", " ")
      ncatt_put(ncnew, "chlorophyll", "standard_name", ncatt_get(x1, "chlorophyll")$standard_name)
      valid_put_check(ncnew, ts.curr$chlorophyll, 0, 50, "chlorophyll", y.traj)

      ncatt_put(ncnew, "radiation_wavelength", "long_name", "Backscatter Radiation Wavelength")
      ncatt_put(ncnew, "radiation_wavelength", "observation_type", "measured")
      ncatt_put(ncnew, "radiation_wavelength", "platform", "platform")
      ncatt_put(ncnew, "radiation_wavelength", "standard_name", "radiation_wavelength")

      ncatt_put(ncnew, "backscatter", "accuracy", " ")
      ncatt_put(ncnew, "backscatter", "ancillary_variables", "radiation_wavelength")
      ncatt_put(ncnew, "backscatter", "instrument", "instrument_flbbcd")
      ncatt_put(ncnew, "backscatter", "long_name", "Optical Backscatter")
      ncatt_put(ncnew, "backscatter", "observation_type", "calculated")
      ncatt_put(ncnew, "backscatter", "platform", "platform")
      ncatt_put(ncnew, "backscatter", "precision", " ")
      ncatt_put(ncnew, "backscatter", "resolution", " ")
      ncatt_put(ncnew, "backscatter", "standard_name", #ncatt_get(x1, "backscatter")$standard_name
                "volume_backwards_scattering_coefficient_of_radiative_flux_in_sea_water")




      ncatt_put(ncnew, "profile_id", "comment",
                paste("Sequential profile number within the trajectory.",
                      "This value is unique in each file that is part of a single trajectory/deployment."))
      ncatt_put(ncnew, "profile_id", "long_name", "Profile ID")
      valid_put_check(ncnew, profile.curr$profile, 0, 2147483647, "profile_id", y.traj)

      ncatt_put(ncnew, "profile_time", "comment", "Timestamp corresponding to the mid-point of the profile")
      ncatt_put(ncnew, "profile_time", "long_name", "Profile Center Time")
      ncatt_put(ncnew, "profile_time", "observation_type", "calculated")
      ncatt_put(ncnew, "profile_time", "platform", "platform")
      ncatt_put(ncnew, "profile_time", "standard_name", ncatt_get(x2, "time")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "profile_time_qc", "profile_time", ncatt_get(x2, "time")$standard_name)

      ncatt_put(ncnew, "profile_lat", "comment",
                "Value is interpolated to provide an estimate of the latitude at the mid-point of the profile")
      ncatt_put(ncnew, "profile_lat", "long_name", "Profile Center Latitude")
      ncatt_put(ncnew, "profile_lat", "observation_type", "calculated")
      ncatt_put(ncnew, "profile_lat", "platform", "platform")
      ncatt_put(ncnew, "profile_lat", "standard_name", ncatt_get(x2, "latitude")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "profile_lat_qc", "profile_lat", ncatt_get(x2, "latitude")$standard_name)
      valid_put_check(ncnew, profile.curr$latitude, -90, 90, "profile_lat", y.traj)

      ncatt_put(ncnew, "profile_lon", "comment",
                "Value is interpolated to provide an estimate of the longitude at the mid-point of the profile")
      ncatt_put(ncnew, "profile_lon", "long_name", "Profile Center Longitude")
      ncatt_put(ncnew, "profile_lon", "observation_type", "calculated")
      ncatt_put(ncnew, "profile_lon", "platform", "platform")
      ncatt_put(ncnew, "profile_lon", "standard_name", ncatt_get(x2, "longitude")$standard_name)
      amlr_ngdac_nc_put_qc(ncnew, "profile_lon_qc", "profile_lon", ncatt_get(x2, "longitude")$standard_name)
      valid_put_check(ncnew, profile.curr$longitude, -180, 180, "profile_lon", y.traj)

      ncatt_put(ncnew, "time_uv", "calendar", "gregorian")
      ncatt_put(ncnew, "time_uv", "comment",
                paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                      "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
      ncatt_put(ncnew, "time_uv", "long_name", "Depth-Averaged Time")
      ncatt_put(ncnew, "time_uv", "observation_type", "calculated")
      ncatt_put(ncnew, "time_uv", "standard_name", "time")
      amlr_ngdac_nc_put_qc(ncnew, "time_uv_qc", "time_uv", "time")

      ncatt_put(ncnew, "lat_uv", "comment",
                paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                      "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
      ncatt_put(ncnew, "lat_uv", "long_name", "Depth-Averaged Latitude")
      ncatt_put(ncnew, "lat_uv", "observation_type", "calculated")
      ncatt_put(ncnew, "lat_uv", "platform", "platform")
      ncatt_put(ncnew, "lat_uv", "standard_name", "latitude")
      amlr_ngdac_nc_put_qc(ncnew, "lat_uv_qc", "lat_uv", "latitude")
      valid_put_check(ncnew, NA, -90, 90, "lat_uv", y.traj)

      ncatt_put(ncnew, "lon_uv", "comment",
                paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                      "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
      ncatt_put(ncnew, "lon_uv", "long_name", "Depth-Averaged Longitude")
      ncatt_put(ncnew, "lon_uv", "observation_type", "calculated")
      ncatt_put(ncnew, "lon_uv", "platform", "platform")
      ncatt_put(ncnew, "lon_uv", "standard_name", "longitude")
      amlr_ngdac_nc_put_qc(ncnew, "lon_uv_qc", "lon_uv", "longitude")
      valid_put_check(ncnew, NA, -180, 180, "lon_uv", y.traj)

      ncatt_put(ncnew, "u", "comment",
                paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                      "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
      ncatt_put(ncnew, "u", "long_name", "Depth-Averaged Eastward Sea Water Velocity")
      ncatt_put(ncnew, "u", "observation_type", "calculated")
      ncatt_put(ncnew, "u", "platform", "platform")
      ncatt_put(ncnew, "u", "standard_name", "eastward_sea_water_velocity")
      amlr_ngdac_nc_put_qc(ncnew, "u_qc", "u", "eastward_sea_water_velocity")
      valid_put_check(ncnew, NA, -10, 10, "u", y.traj)

      ncatt_put(ncnew, "v", "comment",
                paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                      "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
      ncatt_put(ncnew, "v", "long_name", "Depth-Averaged Northward Sea Water Velocity")
      ncatt_put(ncnew, "v", "observation_type", "calculated")
      ncatt_put(ncnew, "v", "platform", "platform")
      ncatt_put(ncnew, "v", "standard_name", "northward_sea_water_velocity")
      amlr_ngdac_nc_put_qc(ncnew, "v_qc", "v", "northward_sea_water_velocity")
      valid_put_check(ncnew, NA, -10, 10, "v", y.traj)



      # See https://gliders.ioos.us/ncei_authority_tables/instruments.txt for NCEI-accepted things
      ncatt_put(ncnew, "platform", "comment", " ")
      ncatt_put(ncnew, "platform", "glider_type", "Teledyne Webb Research Slocum G3 glider")
      ncatt_put(ncnew, "platform", "id", glider.name)
      ncatt_put(ncnew, "platform", "instrument", "instrument_ctd, instrument_flbbcd, instrument_oxygen")
      ncatt_put(ncnew, "platform", "long_name", paste("Slocum Glider", glider.name))
      ncatt_put(ncnew, "platform", "type", "platform")
      ncatt_put(ncnew, "platform", "wmo_id", wmo.id)

      ncatt_put(ncnew, "instrument_ctd", "calibration_date", as.character(ctd.info$calib_date))
      ncatt_put(ncnew, "instrument_ctd", "calibration_report", " ")
      ncatt_put(ncnew, "instrument_ctd", "comment", ctd.comment)
      ncatt_put(ncnew, "instrument_ctd", "factory_calibrated", as.character(ctd.info$calib_date_factory))
      ncatt_put(ncnew, "instrument_ctd", "long_name", "Seabird Glider Payload CTD")
      ncatt_put(ncnew, "instrument_ctd", "make_model", "Seabird GPCTD") #NCEI
      ncatt_put(ncnew, "instrument_ctd", "platform", "platform")
      ncatt_put(ncnew, "instrument_ctd", "serial_number", ctd.info$serial_num)
      ncatt_put(ncnew, "instrument_ctd", "type", "instrument")

      ncatt_put(ncnew, "instrument_flbbcd", "calibration_date", as.character(flbbcd.info$calib_date))
      ncatt_put(ncnew, "instrument_flbbcd", "calibration_report", " ")
      ncatt_put(ncnew, "instrument_flbbcd", "comment", flbbcd.comment)
      ncatt_put(ncnew, "instrument_flbbcd", "factory_calibrated", as.character(flbbcd.info$calib_date_factory))
      ncatt_put(ncnew, "instrument_flbbcd", "long_name", "Optical Backscatter, Chlorophyll, and CDOM Fluorescence Sensor")
      ncatt_put(ncnew, "instrument_flbbcd", "make_model", "WET Labs ECO Puck FLBBCD") #NCEI
      ncatt_put(ncnew, "instrument_flbbcd", "platform", "platform")
      ncatt_put(ncnew, "instrument_flbbcd", "serial_number", flbbcd.info$serial_num)
      ncatt_put(ncnew, "instrument_flbbcd", "type", "instrument")

      ncatt_put(ncnew, "instrument_oxygen", "calibration_date", as.character(oxygen.info$calib_date))
      ncatt_put(ncnew, "instrument_oxygen", "calibration_report", " ")
      ncatt_put(ncnew, "instrument_oxygen", "comment", oxygen.comment)
      ncatt_put(ncnew, "instrument_oxygen", "factory_calibrated", as.character(oxygen.info$calib_date_factory))
      ncatt_put(ncnew, "instrument_oxygen", "long_name", "Dissolved Oxygen Sensor")
      ncatt_put(ncnew, "instrument_oxygen", "make_model", "Aanderaa Optode 4831") #NCEI
      ncatt_put(ncnew, "instrument_oxygen", "platform", "platform")
      ncatt_put(ncnew, "instrument_oxygen", "serial_number", oxygen.info$serial_num)
      ncatt_put(ncnew, "instrument_oxygen", "type", "instrument")



      #----------------------------------------------------
      # Global attributes
      dt.current <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ")
      aerd.url <- "https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center"

      ncatt_put(ncnew, 0, "Conventions", "CF-1.6, COARDS, ACDD-1.3")
      ncatt_put(ncnew, 0, "Metadata_Conventions", "CF-1.6, COARDS, ACDD-1.3")
      ncatt_put(ncnew, 0, "acknowledgement", "This work supported by funding from NOAA")
      ncatt_put(ncnew, 0, "comment", " ")
      ncatt_put(ncnew, 0, "contributor_name", "Christian Reiss, George Watters, Jennifer Walsh, Anthony Cossio, Samuel Woodman")
      ncatt_put(ncnew, 0, "contributor_role", "Principal Investigator, Principal Investigator, Glider Pilot, Glider Pilot, Data Manager")
      ncatt_put(ncnew, 0, "creator_email", "christian.reiss@noaa.gov")
      ncatt_put(ncnew, 0, "creator_institution", "NOAA SWFSC Antarctic Ecosystem Research Division")
      ncatt_put(ncnew, 0, "creator_name", "Christian Reiss")
      ncatt_put(ncnew, 0, "creator_type", "person")
      ncatt_put(ncnew, 0, "creator_url", aerd.url)
      ncatt_put(ncnew, 0, "date_created", dt.current)
      ncatt_put(ncnew, 0, "date_issued", dt.current)
      ncatt_put(ncnew, 0, "date_modified", dt.current)
      ncatt_put(ncnew, 0, "format_version", "IOOS_Glider_NetCDF_v3.0.nc")
      ncatt_put(ncnew, 0, "history",
                paste("Raw glider data processed using the toolbox at https://github.com/socib/glider_toolbox.\n",
                      # "C:\Users\sam.woodman\Documents\R\R-4.0.2\bin\Rscript.exe C:\SMW\ERDDAP\NGDAC\NGDAC_....bat"
                      dt.current, "sam.woodman@noaa.gov of NOAA NMFS SWFSC AERD used R package amlrGlider",
                      paste0("v", packageVersion("amlrGlider")), "(https://github.com/smwoodman/amlrGlider)",
                      "to convert the source file to format_version=IOOS_Glider_NetCDF_v3.0.nc"))
      ncatt_put(ncnew, 0, "id", paste0(y.traj, "-delayed"))
      ncatt_put(ncnew, 0, "institution", "NOAA SWFSC Antarctic Ecosystem Research Division")
      ncatt_put(ncnew, 0, "keywords",
                paste("AUVS > Autonomous Underwater Vehicles, Earth Science > Oceans > Ocean Pressure > Water Pressure,",
                      "Earth Science > Oceans > Ocean Temperature > Water Temperature,",
                      "Earth Science > Oceans > Salinity/Density > Conductivity, Earth Science > Oceans > Salinity/Density > Density,",
                      "Earth Science > Oceans > Salinity/Density > Salinity, glider,",
                      "In Situ Ocean-based platforms > Seaglider, Slocum, Spray, trajectory, underwater glider, water, wmo"))
      ncatt_put(ncnew, 0, "keywords_vocabulary", "GCMD Science Keywords")
      ncatt_put(ncnew, 0, "license",
                paste("This data may be redistributed and used without restriction.",
                      "Data provided as is with no expressed or implied assurance of quality assurance or quality control."))
      ncatt_put(ncnew, 0, "metadata_link", " ")
      ncatt_put(ncnew, 0, "naming_authority", "gov.noaa.fisheries")
      # ncatt_put(ncnew, 0, "platform", "In Situ Ocean-based Platforms > AUVS > Autonomous Underwater Vehicles")
      ncatt_put(ncnew, 0, "platform_type", "Slocum Glider")
      # ncatt_put(ncnew, 0, "platform_vocabulary", "NASA/GCMD Platforms Keywords Version 8.5")
      ncatt_put(ncnew, 0, "processing_level", "No QC has been done to this delayed data")
      ncatt_put(ncnew, 0, "program", "U.S. Antarctic Marine Living Resources Program")
      ncatt_put(ncnew, 0, "project", "FREEBYRD")
      ncatt_put(ncnew, 0, "publisher_email", "christian.reiss@noaa.gov")
      ncatt_put(ncnew, 0, "publisher_institution", "NOAA SWFSC Antarctic Ecosystem Research Division")
      ncatt_put(ncnew, 0, "publisher_name", "Christian Reiss")
      ncatt_put(ncnew, 0, "publisher_url", aerd.url)
      ncatt_put(ncnew, 0, "references", " ")
      ncatt_put(ncnew, 0, "sea_name", "Southern Ocean")
      ncatt_put(ncnew, 0, "source", "Observational data from a profiling glider")
      ncatt_put(ncnew, 0, "standard_name_vocabulary", "CF Standard Name Table v73")
      ncatt_put(ncnew, 0, "summary",
                paste("These data are part of the U.S. Antarctic Marine Living Resources (AMLR) Program Operation FREEBYRD.",
                      "FREEBYRD is a long term program to replace ship-based surveys with autonomous vehicles",
                      "to estimate Antarctic krill biomass in support of the Convention for the",
                      "Conservation of Antarctic Marine Living Resources (CCAMLR).",
                      "This delayed dataset contains CTD, oxygen, chlorophyll a, CDOM and optical backscatter measurements."))
      ncatt_put(ncnew, 0, "title", y.traj)
      ncatt_put(ncnew, 0, "wmo_id", wmo.id)
    }, finally = nc_close(ncnew)) # End of writing ncnew
  } #End of for() loop


  #----------------------------------------------------------------------------
  nc_close(x1)
  nc_close(x2)

  message("Generated ", nrow(x2.df), " nc files in ", file.out.path)
  TRUE
}
