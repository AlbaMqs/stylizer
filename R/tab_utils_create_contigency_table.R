#' Create Contingency Table
#'
#' Generates a contingency table with counts for each combination of `x` and `y`.
#'
#' @param df A dataframe with categorical data.
#' @param x The row variable.
#' @param y The column variable.
#' @import dplyr
#' @importFrom rlang enquo
#' @importFrom tidyr pivot_wider
#'
#' @return A contingency table in wide format.
#' @keywords internal
tab_utils_create_contigency_table <- function(df, x, y) {
  df <- df |>
    dplyr::count(!!rlang::enquo(x), !!rlang::enquo(y)) |>
    tidyr::pivot_wider(
      names_from = rlang::as_name(rlang::enquo(y)),
      values_from = n,
      values_fill = 0
    )
  return(df)
}
