#' Definir valores por defecto del proyecto (perfil)
#' @param defaults lista con campos DwC (ej. institutionCode, collectionCode, ...)
#' @return la lista de defaults activa (invisible)
#' @export
dwc_set_defaults <- function(defaults = list()) {
  stopifnot(is.list(defaults))
  old <- getOption("dwcCOL.defaults", list())
  options("dwcCOL.defaults" = utils::modifyList(old, defaults))
  invisible(getOption("dwcCOL.defaults"))
}

#' Ver defaults activos
#' @export
dwc_get_defaults <- function() getOption("dwcCOL.defaults", list())

#' Borrar defaults activos
#' @export
dwc_clear_defaults <- function() { options("dwcCOL.defaults" = list()); invisible(NULL) }
