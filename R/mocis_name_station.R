#' Translates station from short to long format
#'
#' @param loc, station short form (e.g. ABBE)
#' @return Station long form (e.g. Abbekås)
#' @export
#' @examples
mocis_name_station <- function(loc){
  map_chr(loc, ~ifelse(.x %in% names(stations()[["hav"]]),
                       stations()[["hav"]][[.x]][["name"]],
                       paste(.x, "(full name unavailable)")))
}
