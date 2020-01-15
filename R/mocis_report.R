#' Builds an RMarkdown  file from a mocis_object
#'
#' @param mocis_object, mocis_object A list returned from \code{MoCiS::mocis}
#' @param file, file name/path for output
#' @param contaminants, name of group of contaminants for chapter header (e.g. "metals")
#' @param report_year, year of report
#' @return
#' @export
#' @examples
library(tidyverse)
mocis_report <- function(data, file = "report.Rmd", contaminants, report_year = 2018){
  report_year <- 2018
  data_file <- str_replace(file, "Rmd", "Rdata")
  data <- ifelse(is.da)
  save(mocis_object, file = data_file)
  contents <- mocis_contents(mocis_object) %>%
    mutate(gen_group = case_when(gen == "CLUP" ~ "CLUP",
                                 gen %in% c("GADU", "PERC", "ZOAR") ~ "FISH",
                                 gen %in% c("SIGR", "HAEM", "STER") ~ "EGG")) %>%
    na.omit() %>%
    select(- loc) %>%
    arrange(var, gen) %>%
    distinct() %>%
    split(.$var)
  sink(file = file)
  knitr::knit_expand(file = system.file("rmd", "header.Rmd", package = "MoCiS.tools")) %>% cat(sep = "\n")
  map(contents, mocis_section)
  sink()
}
