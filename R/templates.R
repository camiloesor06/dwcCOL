# Orden EXACTO del output
dwc_col_order <- c(
  "occurrenceID","basisOfRecord","institutionCode","collectionCode",
  "catalogNumber","Other catalogNumber","Old catalogNumber","type","modified",
  "language","accessRights","institutionID","collectionID","occurrenceRemarks",
  "recordNumber","recordedBy","recordedByID","individualCount","establishmentMeans",
  "sex","lifeStage","occurrenceStatus","preparations","disposition","samplingProtocol",
  "samplingEffort","eventDate","year","month","day","habitat","locationID",
  "higherGeography","continent","country","countryCode","stateProvince","county",
  "municipality","locality","verbatimElevation","minimumElevationInMeters",
  "maximumElevationInMeters","verbatimCoordinates","verbatimLatitude",
  "verbatimLongitude","verbatimCoordinateSystem","decimalLatitude","decimalLongitude",
  "geodeticDatum","identifiedBy","identifiedByID","identificationQualifier","taxonID",
  "scientificName","acceptedScientificName","acceptedNameUsage","parentNameUsage",
  "higherClassification","kingdom","phylum","class","order","family","genus",
  "specificEpithet","taxonRank","verbatimTaxonRank","scientificNameAuthorship",
  "vernacularName","nomenclaturalCode","taxonomicStatus","permitType","permitStatus",
  "permitText","Tejido","Caja","Casilla"
)

#' Columnas mínimas requeridas
#' @export
dwc_min_required <- function() c(
  "scientificName","decimalLatitude","decimalLongitude","eventDate","recordedBy"
)

#' Plantilla mínima (solo columnas obligatorias)
#' @export
dwc_min_template <- function(n = 0) {
  tibble::tibble(
    scientificName   = rep(NA_character_, n),
    decimalLatitude  = rep(NA_real_,      n),
    decimalLongitude = rep(NA_real_,      n),
    eventDate        = rep(as.Date(NA),   n),
    recordedBy       = rep(NA_character_, n)
  )
}

# Defaults opcionales
#' @export
dwc_defaults <- function() c(
  language      = "es",
  accessRights  = "Para uso investigativo",
  basisOfRecord = "HumanObservation",
  geodeticDatum = "WGS84"
)

#' @export
dwc_template <- function(n = 0) {
  tibble::as_tibble(setNames(replicate(length(dwc_col_order),
                                       rep(NA_character_, n), simplify = FALSE),
                             dwc_col_order))
}

#' @export
dwc_reorder_and_fill <- function(df) {
  missing <- setdiff(dwc_col_order, names(df))
  if (length(missing)) for (m in missing) df[[m]] <- NA_character_
  df[, dwc_col_order, drop = FALSE]
}

#' @export
dwc_expected_columns <- function() dwc_col_order

