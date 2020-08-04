#' Add values and attributes to nc file
#'
#' Add variable values and both variable and global attributes to nc file
#'
#' @param ncnew open nc file to which to add data and attributes;
#'   output of \code{nc_create}
#' @param x1 open L1 nc file
#' @param x2 open L2 nc file
#' @param profile.curr list of data extracted from AERD L2 file.
#'   This list must contain the following named components: TODO
#' @param ts.curr list of data extracted from AERD L! file
#'   This list must contain the following named components: TODO
#' @param glider.name character; name of glider, e.g. "amlr01"
#' @param wmo.id character; the WMO ID of the glider/deployment
#' @param ctd.info list; CTD information. Must contain the following named elements:
#'   calib_date (Date), calib_date_factory (Date), and serial_num (character)
#' @param optode.info list; oxygen optode information. Must contain the following named elements:
#'   calib_date (Date), calib_date_factory (Date), and serial_num (character)
#' @param flbbcd.info list; flbbcd information. Must contain the following named elements:
#'   calib_date (Date), calib_date_factory (Date), and serial_num (character)
#'
#' @details This function should only be called by \code{\link{amlr_ngdac_convert}}.
#'   It adds attributes, and data as applicable, both globally and to defined variables
#'   for the nc file \code{ncnew}.
#'
#'   Dynamic (aka deployment-specific) information is either calculated from
#'   the data (e.g. deployment date/time)
#'   or passed to function via arguments (e.g. CTD calibration information)
#'
#' @return \code{TRUE} if all code is executed without errors
#'
amlr_ngdac_nc_put <- function(ncnew, x1, x2, profile.curr, ts.curr, glider.name, wmo.id,
                              ctd.info, optode.info, flbbcd.info) {
  stopifnot(
    inherits(ncnew, "ncdf4"),
    inherits(x1, "ncdf4"),
    inherits(profile.curr, "list"),
    identical(names(profile.curr), c("time", "latitude", "longitude", "profile")),
    inherits(ts.curr, "list"),
    identical(names(ts.curr),
              c("time", "depth", "latitude", "longitude",
                "pressure", "temperature", "conductivity", "salinity", "density",
                "profile", "u", "v", "oxygen_saturation", "oxygen_concentration",
                "cdom", "chlorophyll", "backscatter_700")),
    inherits(glider.name, "character"),
    inherits(wmo.id, "character"),
    inherits(ctd.info, "list"),
    inherits(optode.info, "list"),
    inherits(flbbcd.info, "list"),
    identical(names(ctd.info), c("calib_date", "calib_date_factory", "serial_num")),
    identical(names(optode.info), c("calib_date", "calib_date_factory", "serial_num")),
    identical(names(flbbcd.info), c("calib_date", "calib_date_factory", "serial_num")),
    inherits(ctd.info$calib_date, "Date"), inherits(optode.info$calib_date, "Date"), inherits(flbbcd.info$calib_date, "Date"),
    inherits(ctd.info$calib_date_factory, "Date"), inherits(optode.info$calib_date_factory, "Date"),
    inherits(flbbcd.info$calib_date_factory, "Date"),
    inherits(ctd.info$serial_num, "character"), inherits(optode.info$serial_num, "character"), inherits(flbbcd.info$serial_num, "character")
  )


  #----------------------------------------------------------------------------
  ### Add data to variables
   y.traj <- paste0(
    glider.name, "-", format(min(ts.curr$time, na.rm = TRUE), format = "%Y%m%dT%H%M")
  )

  # Time series variables
  ts.count <- length(ts.curr$time)
  qc.val <- rep(as.integer(0), ts.count)
  #TODO ^: should any values be something other than no qc?

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
  # ncvar_put(ncnew, "time_uv", NA)
  # ncvar_put(ncnew, "lat_uv", NA)
  # ncvar_put(ncnew, "lon_uv", NA)
  # ncvar_put(ncnew, "u", NA)
  # ncvar_put(ncnew, "v", NA)

  ncvar_put(ncnew, "profile_time_qc", 0)
  ncvar_put(ncnew, "profile_lat_qc", 0)
  ncvar_put(ncnew, "profile_lon_qc", 0)
  # ncvar_put(ncnew, "time_uv_qc", NA)
  # ncvar_put(ncnew, "lat_uv_qc", NA)
  # ncvar_put(ncnew, "lon_uv_qc", NA)
  # ncvar_put(ncnew, "u_qc", NA)
  # ncvar_put(ncnew, "v_qc", NA)

  # Container variables 'platform' and 'instrument_ctd' have no values, just attributes



  #----------------------------------------------------------------------------
  ### Add attributes to variables

  ncatt_put(ncnew, "time", "ancillary_variables", "time_qc")
  ncatt_put(ncnew, "time", "comment", "Measured or calculated time at each point in the time-series")
  ncatt_put(ncnew, "time", "long_name", "Time")
  ncatt_put(ncnew, "time", "observation_type", "measured")
  ncatt_put(ncnew, "time", "standard_name", ncatt_get(x1, "time")$standard_name)
  amlr_ngdac_nc_put_qc(ncnew, "time_qc", ncatt_get(x1, "time")$standard_name)

  ncatt_put(ncnew, "trajectory", "cf_role", "trajectory_id")
  ncatt_put(ncnew, "trajectory", "comment", "A trajectory is a single deployment of a glider and may span multiple data files.")
  ncatt_put(ncnew, "trajectory", "long_name", "Trajectory/Deployment Name")

  ncatt_put(ncnew, "lat", "ancillary_variables", "lat_qc")
  ncatt_put(ncnew, "lat", "comment", "Value is interpolated to provide an estimate of the latitude at the mid-point of the profile.")
  ncatt_put(ncnew, "lat", "coordinate_reference_frame", "urn:ogc:crs:EPSG::4326")
  ncatt_put(ncnew, "lat", "long_name", "Latitude")
  ncatt_put(ncnew, "lat", "observation_type", "measured")
  ncatt_put(ncnew, "lat", "platform", "platform")
  ncatt_put(ncnew, "lat", "reference", "WGS84")
  ncatt_put(ncnew, "lat", "standard_name", ncatt_get(x1, "latitude")$standard_name)
  amlr_ngdac_nc_put_qc(ncnew, "lat_qc", ncatt_get(x1, "latitude")$standard_name)
  valid_put_check(ncnew, ts.curr$latitude, -90, 90, "lat", y.traj)

  ncatt_put(ncnew, "lon", "ancillary_variables", "lon_qc")
  ncatt_put(ncnew, "lon", "comment", "Value is interpolated to provide an estimate of the longitude at the mid-point of the profile.")
  ncatt_put(ncnew, "lon", "coordinate_reference_frame", "urn:ogc:crs:EPSG::4326")
  ncatt_put(ncnew, "lon", "long_name", "Longitude")
  ncatt_put(ncnew, "lon", "observation_type", "measured")
  ncatt_put(ncnew, "lon", "platform", "platform")
  ncatt_put(ncnew, "lon", "reference", "WGS84")
  ncatt_put(ncnew, "lon", "standard_name", ncatt_get(x1, "longitude")$standard_name)
  amlr_ngdac_nc_put_qc(ncnew, "lon_qc", ncatt_get(x1, "longitude")$standard_name)
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
  amlr_ngdac_nc_put_qc(ncnew, "pressure_qc", "sea_water_pressure")
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
  amlr_ngdac_nc_put_qc(ncnew, "depth_qc", ncatt_get(x1, "depth")$standard_name)
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
  amlr_ngdac_nc_put_qc(ncnew, "temperature_qc", ncatt_get(x1, "temperature")$standard_name)
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
  amlr_ngdac_nc_put_qc(ncnew, "conductivity_qc", "sea_water_electrical_conductivity")
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
  amlr_ngdac_nc_put_qc(ncnew, "salinity_qc", "sea_water_salinity")
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
  amlr_ngdac_nc_put_qc(ncnew, "density_qc", ncatt_get(x1, "density")$standard_name)
  valid_put_check(ncnew, ts.curr$density, 1015.0, 1040.0, "density", y.traj)

  ncatt_put(ncnew, "oxygen_saturation", "accuracy", " ")
  # ncatt_put(ncnew, "oxygen_saturation", "ancillary_variables", "density_qc")
  ncatt_put(ncnew, "oxygen_saturation", "instrument", "instrument_optode")
  ncatt_put(ncnew, "oxygen_saturation", "long_name", "Oxygen saturation")
  ncatt_put(ncnew, "oxygen_saturation", "observation_type", "measured")
  ncatt_put(ncnew, "oxygen_saturation", "platform", "platform")
  ncatt_put(ncnew, "oxygen_saturation", "precision", " ")
  ncatt_put(ncnew, "oxygen_saturation", "resolution", " ")
  ncatt_put(ncnew, "oxygen_saturation", "standard_name", ncatt_get(x1, "oxygen_saturation")$standard_name)
  # amlr_ngdac_nc_put_qc(ncnew, "oxygen_saturation_qc", ncatt_get(x1, "oxygen_saturation")$standard_name)
  valid_put_check(ncnew, ts.curr$oxygen_saturation, 0, 120, "oxygen_saturation", y.traj)

  ncatt_put(ncnew, "oxygen_concentration", "accuracy", " ")
  # ncatt_put(ncnew, "oxygen_concentration", "ancillary_variables", "density_qc")
  ncatt_put(ncnew, "oxygen_concentration", "instrument", "instrument_optode")
  ncatt_put(ncnew, "oxygen_concentration", "long_name", "Oxygen concentration")
  ncatt_put(ncnew, "oxygen_concentration", "observation_type", "calculated")
  ncatt_put(ncnew, "oxygen_concentration", "platform", "platform")
  ncatt_put(ncnew, "oxygen_concentration", "precision", " ")
  ncatt_put(ncnew, "oxygen_concentration", "resolution", " ")
  ncatt_put(ncnew, "oxygen_concentration", "standard_name", ncatt_get(x1, "oxygen_concentration")$standard_name)
  # amlr_ngdac_nc_put_qc(ncnew, "oxygen_concentration_qc", ncatt_get(x1, "oxygen_concentration")$standard_name)
  valid_put_check(ncnew, ts.curr$oxygen_concentration, 0, 500, "oxygen_concentration", y.traj)

  ncatt_put(ncnew, "cdom", "accuracy", " ")
  # ncatt_put(ncnew, "cdom", "ancillary_variables", "density_qc")
  ncatt_put(ncnew, "cdom", "instrument", "instrument_flbbcd")
  ncatt_put(ncnew, "cdom", "long_name", "CDOM")
  ncatt_put(ncnew, "cdom", "observation_type", "measured")
  ncatt_put(ncnew, "cdom", "platform", "platform")
  ncatt_put(ncnew, "cdom", "precision", " ")
  ncatt_put(ncnew, "cdom", "resolution", " ")
  ncatt_put(ncnew, "cdom", "standard_name", #ncatt_get(x1, "cdom")$standard_name
            "concentration_of_colored_dissolved_organic_matter_in_sea_water_expressed_as_equivalent_mass_fraction_of_quinine_sulfate_dihydrate")
  # amlr_ngdac_nc_put_qc(ncnew, "cdom_qc", ncatt_get(x1, "cdom")$standard_name)
  valid_put_check(ncnew, ts.curr$cdom, 0, 375, "cdom", y.traj)

  ncatt_put(ncnew, "chlorophyll", "accuracy", " ")
  # ncatt_put(ncnew, "chlorophyll", "ancillary_variables", "density_qc")
  ncatt_put(ncnew, "chlorophyll", "instrument", "instrument_flbbcd")
  ncatt_put(ncnew, "chlorophyll", "long_name", "Chlorophyll")
  ncatt_put(ncnew, "chlorophyll", "observation_type", "measured")
  ncatt_put(ncnew, "chlorophyll", "platform", "platform")
  ncatt_put(ncnew, "chlorophyll", "precision", " ")
  ncatt_put(ncnew, "chlorophyll", "resolution", " ")
  ncatt_put(ncnew, "chlorophyll", "standard_name", ncatt_get(x1, "chlorophyll")$standard_name)
  # amlr_ngdac_nc_put_qc(ncnew, "chlorophyll_qc", ncatt_get(x1, "chlorophyll")$standard_name)
  valid_put_check(ncnew, ts.curr$chlorophyll, 0, 50, "chlorophyll", y.traj)

  ncatt_put(ncnew, "radiation_wavelength", "long_name", "Backscatter Radiation Wavelength")
  ncatt_put(ncnew, "radiation_wavelength", "observation_type", "measured")
  ncatt_put(ncnew, "radiation_wavelength", "platform", "platform")
  ncatt_put(ncnew, "radiation_wavelength", "standard_name", "radiation_wavelength")

  ncatt_put(ncnew, "backscatter", "accuracy", " ")
  ncatt_put(ncnew, "backscatter", "ancillary_variables", "radiation_wavelength") #"density_qc"
  ncatt_put(ncnew, "backscatter", "instrument", "instrument_flbbcd")
  ncatt_put(ncnew, "backscatter", "long_name", "Optical Backscatter")
  ncatt_put(ncnew, "backscatter", "observation_type", "calculated")
  ncatt_put(ncnew, "backscatter", "platform", "platform")
  ncatt_put(ncnew, "backscatter", "precision", " ")
  ncatt_put(ncnew, "backscatter", "resolution", " ")
  ncatt_put(ncnew, "backscatter", "standard_name", #ncatt_get(x1, "backscatter")$standard_name
            "volume_backwards_scattering_coefficient_of_radiative_flux_in_sea_water")
  # amlr_ngdac_nc_put_qc(ncnew, "backscatter_qc", ncatt_get(x1, "backscatter")$standard_name)
  # valid_put_check(ncnew, ts.curr$backscatter, 0, 375, "backscatter", y.traj)




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
  amlr_ngdac_nc_put_qc(ncnew, "profile_time_qc", ncatt_get(x2, "time")$standard_name)

  ncatt_put(ncnew, "profile_lat", "comment",
            "Value is interpolated to provide an estimate of the latitude at the mid-point of the profile")
  ncatt_put(ncnew, "profile_lat", "long_name", "Profile Center Latitude")
  ncatt_put(ncnew, "profile_lat", "observation_type", "calculated")
  ncatt_put(ncnew, "profile_lat", "platform", "platform")
  ncatt_put(ncnew, "profile_lat", "standard_name", ncatt_get(x2, "latitude")$standard_name)
  amlr_ngdac_nc_put_qc(ncnew, "profile_lat_qc", ncatt_get(x2, "latitude")$standard_name)
  valid_put_check(ncnew, profile.curr$latitude, -90, 90, "profile_lat", y.traj)

  ncatt_put(ncnew, "profile_lon", "comment",
            "Value is interpolated to provide an estimate of the longitude at the mid-point of the profile")
  ncatt_put(ncnew, "profile_lon", "long_name", "Profile Center Longitude")
  ncatt_put(ncnew, "profile_lon", "observation_type", "calculated")
  ncatt_put(ncnew, "profile_lon", "platform", "platform")
  ncatt_put(ncnew, "profile_lon", "standard_name", ncatt_get(x2, "longitude")$standard_name)
  amlr_ngdac_nc_put_qc(ncnew, "profile_lon_qc", ncatt_get(x2, "longitude")$standard_name)
  valid_put_check(ncnew, profile.curr$longitude, -180, 180, "profile_lon", y.traj)

  ncatt_put(ncnew, "time_uv", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "time_uv", "long_name", "Depth-Averaged Time")
  ncatt_put(ncnew, "time_uv", "observation_type", "calculated")
  ncatt_put(ncnew, "time_uv", "standard_name", "time")
  amlr_ngdac_nc_put_qc(ncnew, "time_uv_qc", "time")

  ncatt_put(ncnew, "lat_uv", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "lat_uv", "long_name", "Depth-Averaged Latitude")
  ncatt_put(ncnew, "lat_uv", "observation_type", "calculated")
  ncatt_put(ncnew, "lat_uv", "platform", "platform")
  ncatt_put(ncnew, "lat_uv", "standard_name", "latitude")
  amlr_ngdac_nc_put_qc(ncnew, "lat_uv_qc", "latitude")
  valid_put_check(ncnew, NA, -90, 90, "lat_uv", y.traj)

  ncatt_put(ncnew, "lon_uv", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "lon_uv", "long_name", "Depth-Averaged Longitude")
  ncatt_put(ncnew, "lon_uv", "observation_type", "calculated")
  ncatt_put(ncnew, "lon_uv", "platform", "platform")
  ncatt_put(ncnew, "lon_uv", "standard_name", "longitude")
  amlr_ngdac_nc_put_qc(ncnew, "lon_uv_qc", "longitude")
  valid_put_check(ncnew, NA, -180, 180, "lon_uv", y.traj)

  ncatt_put(ncnew, "u", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "u", "long_name", "Depth-Averaged Eastward Sea Water Velocity")
  ncatt_put(ncnew, "u", "observation_type", "calculated")
  ncatt_put(ncnew, "u", "platform", "platform")
  ncatt_put(ncnew, "u", "standard_name", "eastward_sea_water_velocity")
  amlr_ngdac_nc_put_qc(ncnew, "u_qc", "eastward_sea_water_velocity")
  valid_put_check(ncnew, NA, -10, 10, "u", y.traj)

  ncatt_put(ncnew, "v", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "v", "long_name", "Depth-Averaged Northward Sea Water Velocity")
  ncatt_put(ncnew, "v", "observation_type", "calculated")
  ncatt_put(ncnew, "v", "platform", "platform")
  ncatt_put(ncnew, "v", "standard_name", "northward_sea_water_velocity")
  amlr_ngdac_nc_put_qc(ncnew, "v_qc", "northward_sea_water_velocity")
  valid_put_check(ncnew, NA, -10, 10, "v", y.traj)




  ncatt_put(ncnew, "platform", "comment", paste("Slocum Glider", glider.name))
  ncatt_put(ncnew, "platform", "id", glider.name)
  ncatt_put(ncnew, "platform", "instrument", c("instrument_ctd", "instrument_optode", "instrument_flbbcd"))
  ncatt_put(ncnew, "platform", "long_name", paste("AERD Slocum Glider", glider.name))
  ncatt_put(ncnew, "platform", "type", "platform")
  ncatt_put(ncnew, "platform", "wmo_id", wmo.id)

  ncatt_put(ncnew, "instrument_ctd", "calibration_date", ctd.info$calib_date)
  ncatt_put(ncnew, "instrument_ctd", "calibration_report", " ")
  ncatt_put(ncnew, "instrument_ctd", "comment", "pumped CTD")
  ncatt_put(ncnew, "instrument_ctd", "factory_calibrated", ctd.info$calib_date_factory)
  ncatt_put(ncnew, "instrument_ctd", "make_model", "Seabird GPCTD")
  ncatt_put(ncnew, "instrument_ctd", "long_name", "Seabird Glider Payload CTD")
  ncatt_put(ncnew, "instrument_ctd", "platform", "platform")
  ncatt_put(ncnew, "instrument_ctd", "serial_number", ctd.info$serial_num)
  ncatt_put(ncnew, "instrument_ctd", "type", "instrument")

  ncatt_put(ncnew, "instrument_optode", "calibration_date", optode.info$calib_date)
  ncatt_put(ncnew, "instrument_optode", "calibration_report", " ")
  ncatt_put(ncnew, "instrument_optode", "comment", " ")
  ncatt_put(ncnew, "instrument_optode", "factory_calibrated", optode.info$calib_date_factory)
  ncatt_put(ncnew, "instrument_optode", "make_model", "Aanderaa Oxygen Optode 4831")
  ncatt_put(ncnew, "instrument_optode", "long_name", "Dissolved Oxygen Sensor")
  ncatt_put(ncnew, "instrument_optode", "platform", "platform")
  ncatt_put(ncnew, "instrument_optode", "serial_number", optode.info$serial_num)
  ncatt_put(ncnew, "instrument_optode", "type", "instrument")

  ncatt_put(ncnew, "instrument_flbbcd", "calibration_date", flbbcd.info$calib_date)
  ncatt_put(ncnew, "instrument_flbbcd", "calibration_report", " ")
  ncatt_put(ncnew, "instrument_flbbcd", "comment", " ")
  ncatt_put(ncnew, "instrument_flbbcd", "factory_calibrated", flbbcd.info$calib_date_factory)
  ncatt_put(ncnew, "instrument_flbbcd", "make_model", " ")
  ncatt_put(ncnew, "instrument_flbbcd", "long_name", " ")
  ncatt_put(ncnew, "instrument_flbbcd", "platform", "platform")
  ncatt_put(ncnew, "instrument_flbbcd", "serial_number", flbbcd.info$serial_num)
  ncatt_put(ncnew, "instrument_flbbcd", "type", "instrument")



  #----------------------------------------------------------------------------
  # Global attributes
  date.curr <- paste0(Sys.Date(), "T", format(Sys.time(), format = "%H:%M:%S"), "Z")
  aerd.url <- "https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center"

  ncatt_put(ncnew, 0, "Conventions", "CF-1.6, COARDS, ACDD-1.3")
  ncatt_put(ncnew, 0, "Metadata_Conventions", "CF-1.6, COARDS, ACDD-1.3")
  ncatt_put(ncnew, 0, "acknowledegment", "This work supported by funding from NOAA")
  ncatt_put(ncnew, 0, "comment", "todo")
  ncatt_put(ncnew, 0, "contributor_name", "Christian Reiss, add others, Sam Woodman")
  ncatt_put(ncnew, 0, "contributor_role", "Principal Investigator, todo, Data Manager")
  ncatt_put(ncnew, 0, "creator_email", "christian.reiss@noaa.gov")
  ncatt_put(ncnew, 0, "creator_name", "Christian Reiss")
  ncatt_put(ncnew, 0, "creator_url", aerd.url)
  ncatt_put(ncnew, 0, "date_created", date.curr)
  ncatt_put(ncnew, 0, "date_issued", date.curr)
  ncatt_put(ncnew, 0, "date_modified", date.curr)
  ncatt_put(ncnew, 0, "format_version", "IOOS_Glider_NetCDF_v2.0.nc")
  ncatt_put(ncnew, 0, "history",
            paste("TODO: Raw glider data processed using the toolbox at https://github.com/socib/glider_toolbox.\n",
                  date.curr, "sam.woodman@noaa.gov of NOAA NMFS SWFSC AERD used Convert_amlr.R script",
                  "to convert the source file to format_version=IOOS_Glider_NetCDF_v2.0.nc"))
  ncatt_put(ncnew, 0, "id", paste0(y.traj, "-delayed"))
  ncatt_put(ncnew, 0, "institution", "Antarctic Ecosystem Research Division")
  # ncatt_put(ncnew, 0, "ioos_regional_association", "todo")
  ncatt_put(ncnew, 0, "keywords",
            paste("AUVS > Autonomous Underwater Vehicles, Earth Science > Oceans > Ocean Pressure > Water Pressure,",
                  "Earth Science > Oceans > Ocean Temperature > Water Temperature,",
                  "Earth Science > Oceans > Salinity/Density > Conductivity, Earth Science > Oceans > Salinity/Density > Density,",
                  "Earth Science > Oceans > Salinity/Density > Salinity, glider,",
                  "In Situ Ocean-based platforms > Seaglider, Slocum, Spray, trajectory, underwater glider, water, wmo"))
  ncatt_put(ncnew, 0, "keywords_vocabulary", "GCMD Science Keywords")
  ncatt_put(ncnew, 0, "license",
            paste("The data may be used and redistributed for free but is not intended for legal use,",
                  "since it may contain inaccuracies.",
                  "No person or group associated with this data makes any warranty, express or implied,",
                  "including warranties of merchantability and fitness for a particular purpose,",
                  "or assumes any legal liability for the accuracy, completeness, or usefulness, of this information."))
  ncatt_put(ncnew, 0, "metadata_link", " ")
  ncatt_put(ncnew, 0, "naming_authority", "gov.noaa.fisheries")
  ncatt_put(ncnew, 0, "platform_type", "Slocum Glider")
  ncatt_put(ncnew, 0, "processing_level", "No QC has been done to this delayed data")
  ncatt_put(ncnew, 0, "project", "todo")
  ncatt_put(ncnew, 0, "publisher_email", "christian.reiss@noaa.gov")
  ncatt_put(ncnew, 0, "publisher_name", "Antarctic Ecosystem Research Division")
  ncatt_put(ncnew, 0, "publisher_url", aerd.url)
  ncatt_put(ncnew, 0, "references", "todo")
  ncatt_put(ncnew, 0, "sea_name", "Southern Ocean")
  ncatt_put(ncnew, 0, "source", "Observational data from a profiling glider")
  ncatt_put(ncnew, 0, "standard_name_vocabulary", "CF Standard Name Table v73")
  ncatt_put(ncnew, 0, "summary", "Slocum glider profile data from NOAA NMFS SWFSC Antarctic Ecosystem Research Division")
  ncatt_put(ncnew, 0, "title", "todo")
  ncatt_put(ncnew, 0, "wmo_id", "todo")

  #----------------------------------------------------------------------------
  TRUE
}




amlr_ngdac_nc_put_qc <- function(ncnew, z1, z2) {
  # Add attributes to _qc variables
  # z1: qc variable name, e.g. "lat_qc"
  # z2: descriptor for standard_name, e.g. "latitude
  qc.meanings <- paste("no_qc_performed good_data probably_good_data bad_data_that_are_potentially_correctable",
                       "bad_data value_changed not_used not_used interpolated_value missing_value")
  qc.values <- 0:9

  ncatt_put(ncnew, z1, "flag_meanings", qc.meanings)
  ncatt_put(ncnew, z1, "flag_values", qc.values)
  ncatt_put(ncnew, z1, "long_name", paste(z1, "Quality Flag"))
  ncatt_put(ncnew, z1, "standard_name", paste(z2, "status_flag"))
  ncatt_put(ncnew, z1, "valid_max", as.integer(9))
  ncatt_put(ncnew, z1, "valid_min", as.integer(0))

  TRUE
}




valid_put_check <- function(ncnew, val, v.min, v.max, v.name, y.traj, def.na = -999) {
  # Add valid_min and valid_max attributes. Then
  # Check that values (val) are 1) between valid_min (v.min) and valid_max (v.max)
  #   or 2) the default NA value (def.na) for variable v.name in file y.traj
  ncatt_put(ncnew, v.name, "valid_max", v.max)
  ncatt_put(ncnew, v.name, "valid_min", v.min)

  val.nona <- na.omit(val)
  if (!all(dplyr::between(val.nona, v.min, v.max) | val.nona == def.na)) {
    warning(v.name, " values are not between valid min and max for ", y.traj,
            immediate. = TRUE)
  }
}

# value_check <- function(val, v.min, v.max, v.name, y.traj, def.na = -999) {
#   # Check that values (val) are 1) between valid_min (v.min) and valid_max (v.max)
#   #   or 2) the default NA value (def.na) for variable v.name in file y.traj
#   val.nona <- na.omit(val)
#   if (!all(dplyr::between(val.nona, v.min, v.max) | val.nona == def.na)) {
#     warning(v.name, " values are not between valid min and max for ", y.traj,
#             immediate. = TRUE)
#   }
# }
