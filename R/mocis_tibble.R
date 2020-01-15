#' Places the contents of a mocis_object in a tibble
#'
#' @param mocis_object A list returned from \code{MoCiS::mocis}
#' @return A tibble with
#' @export
#' @examples


mocis_tibble <- function(mocis_object){
  mocis_object %>%
    mocis_contents() %>%
    na.omit() %>%
    mutate(data = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$data),
           aggdata = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$aggdata),
           m = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$m),
           limit = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$limit),
           linmod = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$linmod),
           linmod10 = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$linmod10),
           smooth = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$smooth),
           changepoint = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$changepoint),
           fatadjust = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$fatadjust),
           stattype = pmap(list(loc, gen, var), ~mocis_object[[..1]][[..2]][[..3]]$stattype),
           # Same as data, but with a variable and a value column, exploits that values are kept in the last column of data
           tidydata = map(data, ~mutate(.x, var = last(names(.x))) %>% rename(value = last(names(.x)))),
           # Same as aggdata, but with a variable and a value column, exploits that values are kept in the second column of aggdata
           tidyaggdata = map(aggdata, ~mutate(.x, var = names(.x)[2]) %>% rename(value = names(.x)[2]))
    )
}
