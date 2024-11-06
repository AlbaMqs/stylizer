#' Calculate Percentages
#'
#' Calculates percentages with obfuscation for small values if `out` is set to "obf".
#'
#' @param df A dataframe with counts.
#' @param out Output type: "obf" for obfuscation, "pct" for standard percentage.
#' @import dplyr
#'
#' @return A dataframe with percentages.
#' @keywords internal
calculate_percentages <- function(df, out) {
  pourcentage <- function(n, eff) {
    percent <- (n / sum(eff)) * 100
    paste(formatC(percent, format = "f", digits = 2), "%")
  }

  if (out == "obf") {
    df <- df |>
      dplyr::mutate(across(
        dplyr::where(is.numeric),
        ~ dplyr::case_when(.x >= 5 ~ pourcentage(.x, sum(.x)), .x > 0 ~ "(<5)", .x == 0 ~ "(0)")
      ))
  } else if (out == "pct") {
    df <- df |>
      dplyr::mutate(across(
        dplyr::where(is.numeric),
        ~ pourcentage(.x, sum(.x))
      ))
  }
  return(df)
}
