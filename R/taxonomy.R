
#' Inferir código nomenclatural por reino
#' @keywords internal
dwc_guess_code <- function(kingdom) {
  dplyr::case_when(
    identical(kingdom, "Animalia") ~ "ICZN",
    kingdom %in% c("Plantae", "Fungi") ~ "ICN",
    kingdom %in% c("Bacteria", "Archaea", "Chromista", "Protozoa") ~ "ICNP",
    TRUE ~ NA_character_
  )
}

# %||% si no existe
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

# --- Usage (detalle de un taxón por key) con caché opcional ---
.dwcc_usage <- function(key) {
  tryCatch(rgbif::name_usage(key = as.integer(key), data = "all"), error = function(e) NULL)
}
dwc_usage <- if (requireNamespace("memoise", quietly = TRUE)) memoise::memoise(.dwcc_usage) else .dwcc_usage

# --- backbone para un nombre científico (con authorship del aceptado si aplica) ---
dwc_gbif_backbone_one <- function(x, strict = FALSE) {
  if (is.na(x) || !nzchar(x)) return(NULL)
  res <- tryCatch(rgbif::name_backbone(name = x, strict = strict, verbose = FALSE), error = function(e) NULL)
  if (is.null(res) || (is.list(res) && length(res) == 0)) return(NULL)

  accepted_name <- res$accepted %||% res$species %||% res$scientificName
  accepted_key  <- res$acceptedUsageKey %||% res$speciesKey %||% res$usageKey

  # authorship preferiblemente del aceptado (si existe); si no, del backbone
  acc_auth <- {
    u <- if (!is.null(accepted_key)) dwc_usage(accepted_key) else NULL
    if (!is.null(u) && !is.null(u$data$authorship)) u$data$authorship else res$authorship %||% NA_character_
  }

  sp_ep <- tryCatch(stringr::word(accepted_name, 2), error = function(e) NA_character_)

  tibble::tibble(
    scientificName            = x,
    taxonID                   = as.character(accepted_key %||% NA_character_),
    acceptedScientificName    = accepted_name %||% NA_character_,
    acceptedNameUsage         = accepted_name %||% NA_character_,
    parentNameUsage           = res$genus %||% NA_character_,
    taxonomicStatus           = res$status %||% NA_character_,
    kingdom                   = res$kingdom %||% NA_character_,
    phylum                    = res$phylum %||% NA_character_,
    class                     = res$class %||% NA_character_,
    order                     = res$order %||% NA_character_,
    family                    = res$family %||% NA_character_,
    genus                     = res$genus %||% NA_character_,
    specificEpithet           = sp_ep,
    taxonRank                 = res$rank %||% NA_character_,
    scientificNameAuthorship  = acc_auth,
    higherClassification      = paste(
      na.omit(c(res$kingdom, res$phylum, res$class, res$order, res$family, res$genus, res$species)),
      collapse = " | "
    ),
    nomenclaturalCode         = dwc_guess_code(res$kingdom),
    vernacularName            = NA_character_  # se completa aparte
  )
}

#' Completar taxonomía desde GBIF
#' @export
dwc_taxon_gbif <- function(df, strict = FALSE, cache = TRUE) {
  stopifnot("scientificName" %in% names(df))
  worker <- function(nm) {
    out <- dwc_gbif_backbone_one(nm, strict = strict)
    if (is.null(out)) {
      tibble::tibble(
        scientificName = nm,
        taxonID = NA_character_, acceptedScientificName = NA_character_,
        acceptedNameUsage = NA_character_, parentNameUsage = NA_character_,
        higherClassification = NA_character_, kingdom = NA_character_,
        phylum = NA_character_, class = NA_character_, order = NA_character_,
        family = NA_character_, genus = NA_character_, specificEpithet = NA_character_,
        taxonRank = NA_character_, scientificNameAuthorship = NA_character_,
        vernacularName = NA_character_, nomenclaturalCode = NA_character_,
        taxonomicStatus = NA_character_
      )
    } else out
  }
  if (isTRUE(cache) && requireNamespace("memoise", quietly = TRUE)) worker <- memoise::memoise(worker)

  uniq <- unique(df$scientificName)
  tax_tbl <- purrr::map_dfr(uniq, worker)

  dplyr::left_join(df, tax_tbl, by = "scientificName")
}

# --- Vernacular (nombre común) por taxonID ---
#' Añadir vernacularName desde GBIF (filtrando por idioma y país)
#' @param df data.frame con columna taxonID (GBIF usageKey del aceptado) y scientificName
#' @param lang código de idioma (p.ej., "es")
#' @param country código de país ISO2 (p.ej., "CO") para preferir nombres de Colombia
#' @param prefer_country si TRUE, prioriza nombres con country == CO cuando existan
#' @export
dwc_add_vernacular_gbif <- function(df, lang = "es", country = "CO", prefer_country = TRUE) {
  if (!"taxonID" %in% names(df)) return(df)  # no hay key, no hay lookup

  pick_name <- function(key) {
    if (is.na(key) || !nzchar(key)) return(NA_character_)
    v <- tryCatch(rgbif::name_usage(key = as.integer(key), data = "vernacularNames"), error = function(e) NULL)
    if (is.null(v) || !is.data.frame(v) || nrow(v) == 0) return(NA_character_)

    # Filtra por idioma
    vv <- v[v$language == lang, , drop = FALSE]
    if (nrow(vv) == 0) return(NA_character_)  # sin nombre en ese idioma

    # Si se prefiere país y hay filas con country == CO, filtra
    if (isTRUE(prefer_country) && "country" %in% names(vv)) {
      vv_co <- vv[!is.na(vv$country) & vv$country == country, , drop = FALSE]
      if (nrow(vv_co) > 0) vv <- vv_co
    }

    # Si hay columna preferred, toma esa fila; si no, la primera
    if ("preferred" %in% names(vv) && any(vv$preferred, na.rm = TRUE)) {
      vv <- vv[which(vv$preferred)[1], , drop = FALSE]
    } else {
      vv <- vv[1, , drop = FALSE]
    }
    vv$vernacularName[1] %||% NA_character_
  }

  uniq_keys <- unique(df$taxonID)
  look <- purrr::map_chr(uniq_keys, pick_name)
  vt <- tibble::tibble(taxonID = uniq_keys, vernacularName = look)

  dplyr::left_join(df, vt, by = "taxonID")
}
