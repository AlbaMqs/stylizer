#' Display Counts per Column
#'
#' Adds count information for each level of `y` if `n.col` is `TRUE`.
#'
#' @param df A dataframe with categorical data.
#' @param y The column variable.
#' @param n.col Logical, if `TRUE`, display counts per column.
#' @import dplyr
#' @importFrom rlang enquo
#'
#' @return A dataframe with counts per column level in `y`.
#' @keywords internal
tab_utils_display_ncol <- function(df, y, n.col) {
  if (n.col) {
    df <- df |>
      dplyr::group_by(!!rlang::enquo(y)) |>
      dplyr::mutate(!!rlang::enquo(y) := paste0(!!rlang::enquo(y), "\nN=", dplyr::n())) |>
      dplyr::ungroup()
  }
  return(df)
}
