#' @include templates.R defaults.R
NULL

#' Ensambla la tabla Darwin Core completa desde mínimos (Colombia)
#'
#' Pipeline:
#' - normaliza y valida mínimos
#' - aplica defaults de proyecto/perfil
#' - deriva verbatim coords y georreferencia DANE (municipio/departamento)
#' - (opcional) completa taxonomía desde GBIF y nombre común en ES/CO
#' - (opcional) genera occurrenceID determinístico
#' - fija metadatos y reordena columnas al header estándar
#'
#' @param df_min data.frame con mínimos:
#'   scientificName, decimalLatitude, decimalLongitude, eventDate, recordedBy
#' @param fixed lista con valores por defecto a inyectar/forzar (tiene prioridad sobre el perfil)
#'   Ej.: list(institutionCode="Universidad ICESI", collectionCode="Herpetología Mapiripán")
#' @param use_gbif logical. Completar taxonomía con GBIF (default TRUE)
#' @param gbif_strict logical. Coincidencia estricta en GBIF (default FALSE)
#' @param vernacular_es_co logical. Añadir vernacularName en español priorizando Colombia (default TRUE)
#' @param generate_id logical. Generar occurrenceID determinístico si falta (default FALSE)
#' @param id_prefix prefijo para occurrenceID (default "COL-DWC")
#' @return data.frame con columnas DwC en el orden estándar
#' @export
dwc_build <- function(df_min,
                      fixed = NULL,
                      use_gbif = TRUE,
                      gbif_strict = FALSE,
                      vernacular_es_co = TRUE,
                      generate_id = FALSE,
                      id_prefix = "COL-DWC") {

  # 1) Normalizar y validar mínimos
  df <- dwc_normalize_input(df_min)
  dwc_require_minimum(df)

  # 2) Aplicar defaults (perfil + base + fixed)
  base_defaults <- list(
    institutionCode = "Universidad ICESI",
    collectionCode  = "Herpetología Mapiripán",
    accessRights    = "Para uso investigativo",
    language        = "es",
    geodeticDatum   = "WGS84"
  )
  perfil <- if (exists("dwc_get_defaults")) dwc_get_defaults() else list()
  defaults <- utils::modifyList(base_defaults, perfil)
  if (!is.null(fixed)) defaults <- utils::modifyList(defaults, fixed)

  for (nm in names(defaults)) {
    if (!nm %in% names(df) || all(is.na(df[[nm]]))) {
      df[[nm]] <- defaults[[nm]]
    }
  }

  # 3) Coordenadas verbatim + georreferencia DANE
  df <- df %>%
    dwc_derive_coords() %>%
    dwc_georef_colombia()

  # 4) Taxonomía GBIF (opcional) + nombre común ES/CO
  if (isTRUE(use_gbif)) {
    df <- dwc_taxon_gbif(df, strict = gbif_strict, cache = TRUE)
    if (isTRUE(vernacular_es_co)) {
      df <- dwc_add_vernacular_gbif(df, lang = "es", country = "CO", prefer_country = TRUE)
    }
  }

  # 5) Asegurar columnas para coalesce
  for (nm in c("occurrenceStatus", "type")) {
    if (!nm %in% names(df)) df[[nm]] <- NA_character_
  }

  # 6) occurrenceID determinístico (opcional)
  if (isTRUE(generate_id)) {
    if (!"occurrenceID" %in% names(df)) df$occurrenceID <- NA_character_
    need_id <- is.na(df$occurrenceID) | df$occurrenceID == ""

    # Usa helper del paquete si existe; si no, fallback con {digest}
    gen_fun <- get0("dwc_generate_occurrence_id", mode = "function")
    if (is.function(gen_fun)) {
      df$occurrenceID[need_id] <- gen_fun(df[need_id, , drop = FALSE], prefix = id_prefix)
    } else if (requireNamespace("digest", quietly = TRUE)) {
      key <- paste(
        as.character(df$eventDate[need_id]),
        round(as.numeric(df$decimalLatitude[need_id]),  5),
        round(as.numeric(df$decimalLongitude[need_id]), 5),
        df$scientificName[need_id] %||% "",
        df$recordedBy[need_id]      %||% "",
        df$recordNumber[need_id]    %||% "",
        sep = "|"
      )
      h <- vapply(key, function(k) substr(digest::digest(k, algo = "xxhash64"), 1, 12), character(1))
      df$occurrenceID[need_id] <- paste0(id_prefix, "-", h)
    }
  }

  # 7) Metacampos fijos y consistencia
  df <- df %>%
    dplyr::mutate(
      occurrenceStatus = dplyr::coalesce(.data$occurrenceStatus, "present"),
      modified         = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      type             = dplyr::coalesce(.data$type, "PhysicalObject"),
      continent        = "América del Sur",
      country          = "Colombia",
      countryCode      = "CO"
    )

  # 8) Reorden final al header estándar
  df <- dwc_reorder_and_fill(df)
  df
}

