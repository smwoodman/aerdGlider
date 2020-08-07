amlr_ngdac_nc_put_qc <- function(ncnew, z1, z2) {
  # Add attributes to _qc z1 variable of ncnew
  # z1: qc variable name, e.g. "lat_qc"
  # z2: descriptor for standard_name, e.g. "latitude
  qc.meanings <- paste("no_qc_performed good_data probably_good_data bad_data_that_are_potentially_correctable",
                       "bad_data value_changed not_used not_used interpolated_value missing_value")
  qc.values <- 0:9

  ncatt_put(ncnew, z1, "flag_meanings", qc.meanings)
  ncatt_put(ncnew, z1, "flag_values", qc.values)
  ncatt_put(ncnew, z1, "long_name", paste(z1, "Quality Flag"))
  ncatt_put(ncnew, z1, "standard_name", paste(z2, "status_flag"))
  ncatt_put(ncnew, z1, "valid_max", 9, prec = "byte")
  ncatt_put(ncnew, z1, "valid_min", 0, prec = "byte")

  TRUE
}



valid_put_check <- function(ncnew, val, v.min, v.max, v.name, y.traj, def.na = -999) {
  # Add valid_min and valid_max attributes to ncnew for var v.name. Then
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
