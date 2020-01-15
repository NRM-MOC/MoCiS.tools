#' Translates genus from short to long format
#'
#' @param gen, genus short form (e.g. CLUP)
#' @return Genus long form (e.g. Herring)
#' @export
#' @examples
mocis_name_species <- function(gen){
  map_chr(gen, ~ifelse(.x %in% names(species()),
                       species()[[.x]][["name"]],
                       paste(.x, "(full name unavailable)")))
}
