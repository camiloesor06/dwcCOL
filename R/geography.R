#' Diagnóstico: ¿cuántos polígonos hay cargados?
#' @export
dwc_admin_available <- function() {
  ns <- asNamespace("dwcCOL")  # usa el nombre del paquete explícito
  has_mpio <- exists("col_municipios",    envir = ns, inherits = FALSE)
  has_dep  <- exists("col_departamentos", envir = ns, inherits = FALSE)
  list(
    municipios    = if (has_mpio) nrow(get("col_municipios",    envir = ns)) else 0L,
    departamentos = if (has_dep)  nrow(get("col_departamentos", envir = ns)) else 0L
  )
}

#' Chequeo rápido bbox Colombia
#' @export
dwc_is_in_colombia <- function(lat, lon) {
  lat_ok <- is.finite(lat) & lat >= -4.5 & lat <= 13.8
  lon_ok <- is.finite(lon) & lon >= -82  & lon <= -66
  lat_ok & lon_ok
}

#' Georreferenciar: asignar stateProvince/municipality/locationID (DANE)
#' @param df data.frame con decimalLatitude y decimalLongitude (WGS84)
#' @export
dwc_georef_colombia <- function(df) {
  stopifnot(all(c("decimalLatitude","decimalLongitude") %in% names(df)))
  ns <- asNamespace("dwcCOL")
  if (!exists("col_municipios", envir = ns, inherits = FALSE)) {
    stop("No encuentro 'col_municipios' interno. Corre data-raw/col_admin.R y vuelve a instalar/cargar el paquete.")
  }
  col_municipios <- get("col_municipios", envir = ns, inherits = FALSE)

  pts <- sf::st_as_sf(df, coords = c("decimalLongitude","decimalLatitude"), crs = 4326, remove = FALSE)

  idx_list <- sf::st_within(pts, col_municipios)
  idx <- vapply(idx_list, function(i) if (length(i)) i[[1]] else NA_integer_, integer(1))
  hit <- col_municipios[idx, , drop = FALSE]

  need <- is.na(hit$codigo_dane)
  if (any(need)) {
    nn <- sf::st_nearest_feature(pts[need, ], col_municipios)
    hit[need, ] <- col_municipios[nn, ]
  }

  out <- pts |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      continent     = "América del Sur",
      country       = "Colombia",
      countryCode   = "CO",
      stateProvince = hit$dpto,
      municipality  = hit$mpio,
      county        = NA_character_,
      locationID    = hit$codigo_dane,
      higherGeography = paste("América | Sur América | Colombia |", stateProvince, "|", municipality)
    )

  in_col <- dwc_is_in_colombia(out$decimalLatitude, out$decimalLongitude)
  out[!in_col, c("stateProvince","municipality","locationID","higherGeography")] <- NA_character_
  out
}

