#' todo1
#'
#' todo2
#'
#' @param ncnew nc file to add to
#' @param profile.curr list;
#' @param ts.curr list;
#' @param glider.name name of glider
#'
#' @details
#'
#' @return
#'
#' @examples
#'
#' @export
ngdac_nc_put <- function(ncnew, profile.curr, ts.curr, glider.name) {
  # TODO: add error catch that closes nc file

  #----------------------------------------------------------------------------
  # 'Internal' helper functions

  # Add attributes to _qc variables
  amlr_convert_qc_attr <- function(ncnew, z1, z2) {
    # z1: qc variable name, e.g. "lat_qc"
    # z2: descriptor for standard_name, e.g. "latitude
    qc.meanings <- paste("no_qc_performed good_data probably_good_data bad_data_that_are_potentially_correctable",
                         "bad_data value_changed not_used not_used interpolated_value missing_value")
    qc.values <- 0:9

    ncatt_put(ncnew, z1, "flag_meanings", qc.meanings)
    ncatt_put(ncnew, z1, "flag_values", qc.values)
    ncatt_put(ncnew, z1, "standard_name", paste(z2, "status_flag"))
    ncatt_put(ncnew, z1, "valid_max", as.integer(9))
    ncatt_put(ncnew, z1, "valid_min", as.integer(0))

    TRUE
  }

  # Check that values are between valid_min and valid_max
  amlr_convert_value_check <- function(val, v.min, v.max, v.name, y.traj, def.na = -999) {
    val.nona <- na.omit(val)
    if (!all(dplyr::between(val.nona, v.min, v.max) | val.nona == def.na))
      warning(v.name, " values are not between valid min and max for ", y.traj,
              immediate. = TRUE)
  }


  #----------------------------------------------------------------------------
  # Add data to variables
  y.traj <- paste0(
    glider.name, "-", format(profile.curr$time, format = "%Y%m%d"), "T",
    format(profile.curr$time, format = "%H%M")
  )

  #Time series variables
  ts.count <- length(ts.curr$time)
  qc.val <- rep(as.integer(0), ts.count) #TODO: update if any qc values should be something other than no qc?

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

  ncvar_put(ncnew, "time_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "lat_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "lon_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "pressure_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "depth_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "temperature_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "conductivity_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "salinity_qc", qc.val, start = 1, count = ts.count)
  ncvar_put(ncnew, "density_qc", qc.val, start = 1, count = ts.count)

  #Profile variables (dimensionless)
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

  #Container variables 'platform' and 'instrument_ctd' have no values, just attributes



  #----------------------------------------------------------------------------
  # Add attributes to variables

  ncatt_put(ncnew, "time", "ancillary_variables", "time_qc")
  ncatt_put(ncnew, "time", "comment", "Measured or calculated time at each point in the time-series")
  ncatt_put(ncnew, "time", "observation_type", "measured")
  ncatt_put(ncnew, "time", "standard_name", "time")
  amlr_convert_qc_attr(ncnew, "time_qc", "time")

  ncatt_put(ncnew, "trajectory", "cf_role", "trajectory_id")
  ncatt_put(ncnew, "trajectory", "comment", "A trajectory is a single deployment of a glider and may span multiple data files.")

  ncatt_put(ncnew, "lat", "ancillary_variables", "lat_qc")
  ncatt_put(ncnew, "lat", "comment", "Value is interpolated to provide an estimate of the latitude at the mid-point of the profile.")
  ncatt_put(ncnew, "lat", "coordinate_reference_frame", "urn:ogc:crs:EPSG::4326")
  ncatt_put(ncnew, "lat", "observation_type", "measured")
  ncatt_put(ncnew, "lat", "platform", "platform")
  ncatt_put(ncnew, "lat", "reference", "WGS84")
  ncatt_put(ncnew, "lat", "standard_name", "latitude")
  ncatt_put(ncnew, "lat", "valid_max", 90)
  ncatt_put(ncnew, "lat", "valid_min", -90)
  amlr_convert_qc_attr(ncnew, "lat_qc", "latitude")
  amlr_convert_value_check(ts.curr$latitude, -90, 90, "latitude", y.traj)

  ncatt_put(ncnew, "lon", "ancillary_variables", "lon_qc")
  ncatt_put(ncnew, "lon", "comment", "Value is interpolated to provide an estimate of the longitude at the mid-point of the profile.")
  ncatt_put(ncnew, "lon", "coordinate_reference_frame", "urn:ogc:crs:EPSG::4326")
  ncatt_put(ncnew, "lon", "observation_type", "measured")
  ncatt_put(ncnew, "lon", "platform", "platform")
  ncatt_put(ncnew, "lon", "reference", "WGS84")
  ncatt_put(ncnew, "lon", "standard_name", "longitude")
  ncatt_put(ncnew, "lon", "valid_max", 180)
  ncatt_put(ncnew, "lon", "valid_min", -180)
  amlr_convert_qc_attr(ncnew, "lon_qc", "longitude")
  amlr_convert_value_check(ts.curr$longitude, -180, 180, "longitude", y.traj)

  ncatt_put(ncnew, "pressure", "accuracy", " ")
  ncatt_put(ncnew, "pressure", "ancillary_variables", "pressure_qc")
  ncatt_put(ncnew, "pressure", "comment", " ")
  ncatt_put(ncnew, "pressure", "instrument", "instrument_ctd")
  ncatt_put(ncnew, "pressure", "observation_type", "measured")
  ncatt_put(ncnew, "pressure", "platform", "platform")
  ncatt_put(ncnew, "pressure", "positive", "down")
  ncatt_put(ncnew, "pressure", "precision", " ")
  ncatt_put(ncnew, "pressure", "reference_datum", "sea-surface")
  ncatt_put(ncnew, "pressure", "resolution", " ")
  ncatt_put(ncnew, "pressure", "standard_name", "sea_water_pressure")
  ncatt_put(ncnew, "pressure", "valid_max", 2000)
  ncatt_put(ncnew, "pressure", "valid_min", 0)
  amlr_convert_qc_attr(ncnew, "pressure_qc", "sea_water_pressure")
  amlr_convert_value_check(ts.curr$pressure, 0, 2000, "pressure", y.traj)

  ncatt_put(ncnew, "depth", "accuracy", " ")
  ncatt_put(ncnew, "depth", "ancillary_variables", "depth_qc")
  ncatt_put(ncnew, "depth", "comment", " ")
  ncatt_put(ncnew, "depth", "instrument", "instrument_ctd")
  ncatt_put(ncnew, "depth", "observation_type", "calculated")
  ncatt_put(ncnew, "depth", "platform", "platform")
  ncatt_put(ncnew, "depth", "positive", "down")
  ncatt_put(ncnew, "depth", "precision", " ")
  ncatt_put(ncnew, "depth", "reference_datum", "sea-surface")
  ncatt_put(ncnew, "depth", "resolution", " ")
  ncatt_put(ncnew, "depth", "standard_name", "depth")
  ncatt_put(ncnew, "depth", "valid_max", 2000)
  ncatt_put(ncnew, "depth", "valid_min", 0)
  amlr_convert_qc_attr(ncnew, "depth_qc", "depth")
  amlr_convert_value_check(ts.curr$depth, 0, 2000, "depth", y.traj)

  ncatt_put(ncnew, "temperature", "accuracy", " ")
  ncatt_put(ncnew, "temperature", "ancillary_variables", "temperature_qc")
  ncatt_put(ncnew, "temperature", "instrument", "instrument_ctd")
  ncatt_put(ncnew, "temperature", "observation_type", "measured")
  ncatt_put(ncnew, "temperature", "platform", "platform")
  ncatt_put(ncnew, "temperature", "precision", " ")
  ncatt_put(ncnew, "temperature", "resolution", " ")
  ncatt_put(ncnew, "temperature", "standard_name", "sea_water_temperature")
  ncatt_put(ncnew, "temperature", "valid_max", 40.0)
  ncatt_put(ncnew, "temperature", "valid_min", -5.0)
  amlr_convert_qc_attr(ncnew, "temperature_qc", "sea_water_temperature")
  amlr_convert_value_check(ts.curr$temperature, -5, 40, "temperature", y.traj)

  ncatt_put(ncnew, "conductivity", "accuracy", " ")
  ncatt_put(ncnew, "conductivity", "ancillary_variables", "conductivity_qc")
  ncatt_put(ncnew, "conductivity", "instrument", "instrument_ctd")
  ncatt_put(ncnew, "conductivity", "observation_type", "measured")
  ncatt_put(ncnew, "conductivity", "platform", "platform")
  ncatt_put(ncnew, "conductivity", "precision", " ")
  ncatt_put(ncnew, "conductivity", "resolution", " ")
  ncatt_put(ncnew, "conductivity", "standard_name", "sea_water_electrical_conductivity")
  ncatt_put(ncnew, "conductivity", "valid_max", 10.0)
  ncatt_put(ncnew, "conductivity", "valid_min", 0.0)
  amlr_convert_qc_attr(ncnew, "conductivity_qc", "sea_water_electrical_conductivity")
  amlr_convert_value_check(ts.curr$conductivity, 0, 10, "conductivity", y.traj)

  ncatt_put(ncnew, "salinity", "accuracy", " ")
  ncatt_put(ncnew, "salinity", "ancillary_variables", "salinity_qc")
  ncatt_put(ncnew, "salinity", "instrument", "instrument_ctd")
  ncatt_put(ncnew, "salinity", "observation_type", "calculated")
  ncatt_put(ncnew, "salinity", "platform", "platform")
  ncatt_put(ncnew, "salinity", "precision", " ")
  ncatt_put(ncnew, "salinity", "resolution", " ")
  ncatt_put(ncnew, "salinity", "standard_name", "sea_water_practical_salinity")
  ncatt_put(ncnew, "salinity", "valid_max", 40.0)
  ncatt_put(ncnew, "salinity", "valid_min", 0.0)
  amlr_convert_qc_attr(ncnew, "salinity_qc", "sea_water_salinity")
  amlr_convert_value_check(ts.curr$salinity, 0, 40.0, "salinity", y.traj)

  ncatt_put(ncnew, "density", "accuracy", " ")
  ncatt_put(ncnew, "density", "ancillary_variables", "density_qc")
  ncatt_put(ncnew, "density", "instrument", "instrument_ctd")
  ncatt_put(ncnew, "density", "observation_type", "calculated")
  ncatt_put(ncnew, "density", "platform", "platform")
  ncatt_put(ncnew, "density", "precision", " ")
  ncatt_put(ncnew, "density", "resolution", " ")
  ncatt_put(ncnew, "density", "standard_name", "sea_water_density")
  ncatt_put(ncnew, "density", "valid_max", 1040.0)
  ncatt_put(ncnew, "density", "valid_min", 1015.0)
  amlr_convert_qc_attr(ncnew, "density_qc", "sea_water_density")
  amlr_convert_value_check(ts.curr$density, 1015.0, 1040.0, "density", y.traj)



  ncatt_put(ncnew, "profile_id", "comment",
            paste("Sequential profile number within the trajectory.",
                  "This value is unique in each file that is part of a single trajectory/deployment."))
  ncatt_put(ncnew, "profile_id", "valid_max", 2147483647)
  ncatt_put(ncnew, "profile_id", "valid_min", 1)
  amlr_convert_value_check(profile.curr$profile, 0, 2147483647, "profile_id", y.traj)

  ncatt_put(ncnew, "profile_time", "comment", "Timestamp corresponding to the mid-point of the profile")
  ncatt_put(ncnew, "profile_time", "observation_type", "calculated")
  ncatt_put(ncnew, "profile_time", "platform", "platform")
  ncatt_put(ncnew, "profile_time", "standard_name", "time")
  amlr_convert_qc_attr(ncnew, "profile_time_qc", "time")

  ncatt_put(ncnew, "profile_lat", "comment",
            "Value is interpolated to provide an estimate of the latitude at the mid-point of the profile")
  ncatt_put(ncnew, "profile_lat", "observation_type", "calculated")
  ncatt_put(ncnew, "profile_lat", "platform", "platform")
  ncatt_put(ncnew, "profile_lat", "standard_name", "latitude")
  ncatt_put(ncnew, "profile_lat", "valid_max", 90)
  ncatt_put(ncnew, "profile_lat", "valid_min", -90)
  amlr_convert_qc_attr(ncnew, "profile_lat_qc", "latitude")
  amlr_convert_value_check(profile.curr$latitude, -90, 90, "profile latitude", y.traj)

  ncatt_put(ncnew, "profile_lon", "comment",
            "Value is interpolated to provide an estimate of the longitude at the mid-point of the profile")
  ncatt_put(ncnew, "profile_lon", "observation_type", "calculated")
  ncatt_put(ncnew, "profile_lon", "platform", "platform")
  ncatt_put(ncnew, "profile_lon", "standard_name", "longitude")
  ncatt_put(ncnew, "profile_lon", "valid_max", 180)
  ncatt_put(ncnew, "profile_lon", "valid_min", -1800)
  amlr_convert_qc_attr(ncnew, "profile_lon_qc", "longitude")
  amlr_convert_value_check(profile.curr$longitude, -180, 180, "profile longitude", y.traj)

  ncatt_put(ncnew, "time_uv", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "time_uv", "observation_type", "calculated")
  ncatt_put(ncnew, "time_uv", "standard_name", "time")
  amlr_convert_qc_attr(ncnew, "time_uv_qc", "time")

  ncatt_put(ncnew, "lat_uv", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "lat_uv", "observation_type", "calculated")
  ncatt_put(ncnew, "lat_uv", "platform", "platform")
  ncatt_put(ncnew, "lat_uv", "standard_name", "latitude")
  ncatt_put(ncnew, "lat_uv", "valid_max", 90)
  ncatt_put(ncnew, "lat_uv", "valid_min", -90)
  amlr_convert_qc_attr(ncnew, "lat_uv_qc", "latitude")
  # amlr_convert_value_check(NA, -90, 90, "lat_uv", y.traj)

  ncatt_put(ncnew, "lon_uv", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "lon_uv", "observation_type", "calculated")
  ncatt_put(ncnew, "lon_uv", "platform", "platform")
  ncatt_put(ncnew, "lon_uv", "standard_name", "longitude")
  ncatt_put(ncnew, "lon_uv", "valid_max", 180)
  ncatt_put(ncnew, "lon_uv", "valid_min", -180)
  amlr_convert_qc_attr(ncnew, "lon_uv_qc", "longitude")
  # amlr_convert_value_check(NA, -180, 180, "lon_uv", y.traj)

  ncatt_put(ncnew, "u", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "u", "observation_type", "calculated")
  ncatt_put(ncnew, "u", "platform", "platform")
  ncatt_put(ncnew, "u", "standard_name", "eastward_sea_water_velocity")
  ncatt_put(ncnew, "u", "valid_max", 10)
  ncatt_put(ncnew, "u", "valid_min", -10)
  amlr_convert_qc_attr(ncnew, "u_qc", "eastward_sea_water_velocity")
  # amlr_convert_value_check(NA, -10, 10, "u", y.traj)

  ncatt_put(ncnew, "v", "comment",
            paste("The depth-averaged current is an estimate of the net current measured while the glider is underwater.",
                  "The value is calculated over the entire underwater segment, which may consist of 1 or more dives."))
  ncatt_put(ncnew, "v", "observation_type", "calculated")
  ncatt_put(ncnew, "v", "platform", "platform")
  ncatt_put(ncnew, "v", "standard_name", "northward_sea_water_velocity")
  ncatt_put(ncnew, "v", "valid_max", 10)
  ncatt_put(ncnew, "v", "valid_min", -10)
  amlr_convert_qc_attr(ncnew, "v_qc", "northward_sea_water_velocity")
  # amlr_convert_value_check(NA, -10, 10, "v", y.traj)







  ncatt_put(ncnew, "platform", "comment", "Slocum Glider TODO")
  ncatt_put(ncnew, "platform", "id", "TODO")
  ncatt_put(ncnew, "platform", "instrument", "instrument_ctd")
  ncatt_put(ncnew, "platform", "long_name", paste("AERD Slocum Glider", glider.name))
  ncatt_put(ncnew, "platform", "type", "platform")
  ncatt_put(ncnew, "platform", "wmo_id", "TODO")

  ncatt_put(ncnew, "instrument_ctd", "calibration_date", "TODO")
  ncatt_put(ncnew, "instrument_ctd", "calibration_report", "TODO")
  ncatt_put(ncnew, "instrument_ctd", "comment", "pumped CTD")
  ncatt_put(ncnew, "instrument_ctd", "factory_calibrated", "TODO")
  ncatt_put(ncnew, "instrument_ctd", "make_model", "TODO")
  ncatt_put(ncnew, "instrument_ctd", "long_name", "Seabird Glider Payload CTD")
  ncatt_put(ncnew, "instrument_ctd", "platform", "platform")
  ncatt_put(ncnew, "instrument_ctd", "serial_number", "TODO")
  ncatt_put(ncnew, "instrument_ctd", "type", "platform")



  #----------------------------------------------------------------------------
  # Global attributes
  date.curr <- paste0(Sys.Date(), "T", format(Sys.time(), format = "%H:%M:%S"), "Z")
  aerd.url <- "https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center"

  ncatt_put(ncnew, 0, "CF-1.6, COARDS, ACDD-1.3")
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
