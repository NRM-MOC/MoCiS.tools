#' Translates station from short to long format
#'
#' @param loc, station short form (e.g. ABBE)
#' @return Station long form (e.g. Abbekås)
#' @export
#' @examples
mocis_name_station <- function(loc){
  map_chr(loc, ~dplyr::case_when(.x %in% names(stations()[["hav"]]) ~  stations()[["hav"]][[.x]][["name"]],
                                 .x %in% names(stations()[["limn"]]) ~  stations()[["limn"]][[.x]][["name"]],              
                                 TRUE ~ paste(.x, "(full name unavailable)")))
}