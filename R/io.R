#' Escribir Darwin Core en CSV con el orden estándar
#'
#' @param x data.frame con columnas DwC (en cualquier orden)
#' @param path ruta al archivo .csv
#' @param excel_friendly Si TRUE, escribe con BOM y separador compatible con Excel (write_excel_csv).
#'                       Si FALSE, escribe CSV UTF-8 estándar con coma.
#' @return (invisible) path
#' @export
write_dwc_csv <- function(x, path, excel_friendly = TRUE) {
  stopifnot(is.data.frame(x), is.character(path), length(path) == 1)
  x <- dwc_reorder_and_fill(x)

  if (isTRUE(excel_friendly)) {
    # Agrega BOM y (según configuración regional) separador aceptado por Excel.
    # Útil para Windows/Excel.
    readr::write_excel_csv(x, file = path, na = "")
  } else {
    # CSV UTF-8 puro con coma como separador (recomendado para pipelines)
    readr::write_csv(x, file = path, na = "")
  }
  invisible(path)
}
