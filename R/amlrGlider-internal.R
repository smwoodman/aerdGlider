amlr_ngdac_nc_put_qc <- function(ncnew, z.var, z.long, z.standard) {
  # Add attributes to _qc z.var variable of ncnew
  # z.var: qc variable name, e.g. "lat_qc"
  # z.long: start of long_name
  # z.standard: descriptor for standard_name, e.g. "latitude"
  qc.meanings <- paste("no_qc_performed good_data probably_good_data bad_data_that_are_potentially_correctable",
                       "bad_data value_changed not_used not_used interpolated_value missing_value")
  qc.values <- 0:9

  ncatt_put(ncnew, z.var, "flag_meanings", qc.meanings)
  ncatt_put(ncnew, z.var, "flag_values", qc.values, prec = "byte")
  ncatt_put(ncnew, z.var, "long_name", paste(z.long, "Quality Flag"))
  ncatt_put(ncnew, z.var, "standard_name", paste(z.standard, "status_flag"))
  ncatt_put(ncnew, z.var, "valid_max", 9, prec = "byte")
  ncatt_put(ncnew, z.var, "valid_min", 0, prec = "byte")

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
