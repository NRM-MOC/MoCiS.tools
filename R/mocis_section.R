#' For internal use by mocis_report
#'
#' @param section_contents
#' @return
#' @export
#' @examples
mocis_section <- function(section_contents){
  var <- section_contents$var[1]
  cat(paste("\n\n#", var, "in marine biota\n\n"))
  for (group in unique(section_contents$gen_group)){
      knitr::knit_expand(file = system.file("rmd", paste0(group, "section.Rmd"), package = "MoCiS.tools")) %>% cat(sep = "\n")
  }
}
