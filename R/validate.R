
#' Normaliza entradas mínimas y deriva year/month/day
#' Crea columnas opcionales si faltan (language, accessRights, basisOfRecord, geodeticDatum)
#' Acepta snake_case -> camelCase para mínimos
#' @export
dwc_normalize_input <- function(df_min) {
  # Mapear snake_case -> camelCase si vienen así
  rename_map <- c(
    scientific_name   = "scientificName",
    decimal_latitude  = "decimalLatitude",
    decimal_longitude = "decimalLongitude",
    event_date        = "eventDate",
    recorded_by       = "recordedBy",
    access_rights     = "accessRights",
    basis_of_record   = "basisOfRecord",
    geodetic_datum    = "geodeticDatum"
  )
  present <- intersect(names(rename_map), names(df_min))
  if (length(present)) names(df_min)[match(present, names(df_min))] <- unname(rename_map[present])

  # Crear opcionales si faltan
  for (nm in names(dwc_defaults())) if (!nm %in% names(df_min)) df_min[[nm]] <- NA_character_

  df_min %>%
    dplyr::mutate(
      scientificName   = stringr::str_squish(.data$scientificName),
      decimalLatitude  = as.numeric(.data$decimalLatitude),
      decimalLongitude = as.numeric(.data$decimalLongitude),
      eventDate        = lubridate::as_date(.data$eventDate),
      year             = lubridate::year(.data$eventDate),
      month            = lubridate::month(.data$eventDate),
      day              = lubridate::day(.data$eventDate),
      language         = dplyr::coalesce(.data$language,      dwc_defaults()[["language"]]),
      accessRights     = dplyr::coalesce(.data$accessRights,  dwc_defaults()[["accessRights"]]),
      basisOfRecord    = dplyr::coalesce(.data$basisOfRecord, dwc_defaults()[["basisOfRecord"]]),
      geodeticDatum    = dplyr::coalesce(.data$geodeticDatum, dwc_defaults()[["geodeticDatum"]])
    )
}

#' Verifica columnas mínimas requeridas (existencia y no-NA / no-vacías)
#' @export
dwc_require_minimum <- function(df) {
  req <- c("scientificName","decimalLatitude","decimalLongitude","eventDate","recordedBy")
  miss <- setdiff(req, names(df))
  if (length(miss)) stop("Faltan columnas mínimas: ", paste(miss, collapse = ", "))

  bad <- character(0)

  # character: no NA y no string vacío
  if (any(is.na(df$scientificName) | stringr::str_trim(df$scientificName) == "")) bad <- c(bad, "scientificName")
  if (any(is.na(df$recordedBy)    | stringr::str_trim(df$recordedBy)    == "")) bad <- c(bad, "recordedBy")

  # numéricos/fecha: no NA
  if (any(is.na(df$decimalLatitude)))  bad <- c(bad, "decimalLatitude")
  if (any(is.na(df$decimalLongitude))) bad <- c(bad, "decimalLongitude")
  if (any(is.na(df$eventDate)))        bad <- c(bad, "eventDate")

  if (length(bad)) stop("Columnas mínimas con valores vacíos/NA: ", paste(unique(bad), collapse = ", "))
  invisible(df)
}
