#' Builds a table of summary statistics
#'
#' @param mocis_object A list returned from \code{MoCiS::mocis}
#' @return A tibble with
#' @export
#' @examples


mocis_table <- function(data, dec = 2){
  print_ci <- function(estimate, lower, upper){
    ifelse(is.numeric(estimate),
           paste0(round(estimate, dec), " (", round(lower, dec), ", ", round(upper, dec), ")"),
           "")
  }
  print_value <- function(value){
    ifelse(is.numeric(value),
           paste(round(value, dec)),
           "")
  }
  print_p_value <- function(value){
    ifelse(is.numeric(value),
           ifelse(value > 0.001, paste(round(value, 3)), "<0.001"),
           "")
  }

  data %>%
    transmute(var = var,
              gen = gen,
              loc = loc,
              # Full data
              `Sampling site` = mocis_name_station(loc),
              Species = mocis_name_species(gen),
              `$n_{\\text{obs}}$` = map_dbl(aggdata, ~sum(.x$n)),
              `$n_y$` = map_dbl(aggdata, ~length(unique(.x[["YEAR"]]))),
              Years = map_chr(aggdata, ~paste(range(.x[["YEAR"]]), collapse = "-")),
              `Slope (95$\\%$ CI)` = map_chr(linmod, ~print_ci(.x[["slope"]], .x[["lower"]], .x[["upper"]])),
              `$R^2$` = map_chr(linmod, ~print_value(.x[["r2"]])),
              `$p$` = map_chr(linmod, ~print_p_value(.x[["p"]])),
              CV = map_chr(linmod, ~print_value(.x[["cv"]][1])),
              `LDT` = map_chr(linmod, ~print_value(.x[["cv"]][2])),
              YRQ = map_chr(linmod, ~print_value(.x[["cv"]][3])),
              `Pow$_{\\text{tot}}$` = map_chr(linmod, ~print_value(.x[["power"]][1])),
              `Pow$_{10y}$` = map_chr(linmod, ~print_value(.x[["power"]][2])),
              `LDT$_{10y}$` = map_chr(linmod, ~print_value(.x[["power"]][3])),
              `Conc$_{\\text{pred}}$ (95$\\%$ CI)` = map_chr(linmod, ~print_ci(.x[["yhat.last"]], .x[["yhat.last.lower"]], .x[["yhat.last.upper"]])),
              # Past ten years of data
              `Slope$_{10y}$ (95$\\%$ CI)` = map_chr(linmod10, ~print_ci(.x[["slope"]], .x[["lower"]], .x[["upper"]])),
              `LDT$_{10y}$` = map_chr(linmod10, ~print_value(.x[["cv"]][2])),
              `YRQ$_{10}$` = map_chr(linmod10, ~print_value(.x[["cv"]][3])),
              `Pow$_{tot}$` = map_chr(linmod10, ~print_value(.x[["power"]][1])),
              `$R^2_{10y}$` = map_chr(linmod10, ~print_value(.x[["r2"]])),
              `$p_{10y}$` = map_chr(linmod10, ~print_p_value(.x[["p"]])),
              `Yr$_{\\text{change}}$` = map_chr(changepoint, ~ifelse(is.null(.x), "",.x[["changepoint"]]))
    )
}
