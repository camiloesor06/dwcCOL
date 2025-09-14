
#' Decimal a DMS
#' @export
to_dms <- function(dd, lat = TRUE) {
  hemi <- if (lat) ifelse(dd >= 0, "N", "S") else ifelse(dd >= 0, "E", "W")
  x <- abs(dd); d <- floor(x); mfull <- (x - d)*60; m <- floor(mfull); s <- (mfull - m)*60
  sprintf("%02d %02d %06.3f %s", d, m, s, hemi)
}

#' Derivar campos verbatim de coordenadas
#' Crea geodeticDatum = "WGS84" si no existe
#' Requiere: decimalLatitude, decimalLongitude (WGS84, numéricos o coercibles)
#' @export
dwc_derive_coords <- function(df) {
  stopifnot(all(c("decimalLatitude","decimalLongitude") %in% names(df)))

  # Asegura numéricos por si vienen como texto
  df <- dplyr::mutate(
    df,
    decimalLatitude  = as.numeric(.data$decimalLatitude),
    decimalLongitude = as.numeric(.data$decimalLongitude)
  )

  # Si falta la columna geodeticDatum, créala con WGS84 (default)
  if (!"geodeticDatum" %in% names(df)) {
    df$geodeticDatum <- "WGS84"
  } else {
    df$geodeticDatum <- dplyr::coalesce(df$geodeticDatum, "WGS84")
  }

  dplyr::mutate(
    df,
    verbatimLatitude          = to_dms(decimalLatitude,  lat = TRUE),
    verbatimLongitude         = to_dms(decimalLongitude, lat = FALSE),
    verbatimCoordinates       = paste(verbatimLatitude, verbatimLongitude),
    verbatimCoordinateSystem  = "Grados, minutos, segundos"
  )
}

