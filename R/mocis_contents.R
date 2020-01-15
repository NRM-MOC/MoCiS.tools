#' Extracts table of contents of a MoCiS object and places in data frame
#'
#' @param mocis_object A list returned from \code{MoCiS::mocis}
#' @return A tibble with columns \code{loc} (locationm), \code{var} (variable) and \code{gen} (genus) and one row for each combination found in \code{mocis_object}
#' @export
#' @examples


mocis_contents <- function(mocis_object){
  mocis_object %>%
    map_depth(2, names) %>%
    unlist() %>%
    enframe() %>%
    separate(name, into = c("loc", "gen")) %>%
    mutate(gen = str_sub(gen, 1, 4)) %>%
    rename(var = value) %>%
    arrange(var)
}
