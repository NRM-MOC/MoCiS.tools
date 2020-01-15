#' Gets unit given variable and genus
#'
#' @param var, variable short form (e.g. HG)
#' @param gen, genus short form (e.g. CLUP)
#' @return unit (e.g. bquote(Pb*","~mu*g/g~dw~liver))
#' @export
#' @examples
mocis_get_unit <- function(var, gen){
  labels <- plotlabs()
  if (gen %in% c("CLUP", "PERC", "GADU", "ZOAR", "PERC", "ESOX", "SALV")) {
    return(getElement(getElement(labels, "fish"), var))
  }
  if (gen == "MYTI") {
    return(getElement(getElement(labels, "bluemussel"), var))
  }
  if (gen %in% c("SIGR", "HAEM", "STER")) {
    return(getElement(getElement(labels, "birds"), var))
  }
}
