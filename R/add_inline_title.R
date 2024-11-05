#' Add Inline Title to Contingency Table
#'
#' Adds the row variable title inline in the table if `inline.title` is `TRUE`.
#'
#' @param df A contingency table.
#' @param x The row variable.
#' @param df_init The initial dataframe to retrieve variable labels.
#' @param inline.title Logical, if `TRUE`, add title inline.
#' @param label.title Logical, if `TRUE`, replace row variable title with its label.
#' @param na_count Count of missing values if `na.use` is "as.note".
#'
#' @return A dataframe with the inline title if specified.
#' @keywords internal
add_inline_title <- function(df, x, df_init, inline.title, label.title, na_count) {
  label <- labelled::var_label(df_init[[rlang::as_name(rlang::enquo(x))]])
  if (is.null(label)) label <- rlang::as_name(rlang::enquo(x))
  label <- ifelse(!is.null(na_count), paste0(label, "<missing:", na_count, ">"), label)

  if (inline.title) {
    df <- dplyr::bind_rows(tibble::tibble(!!!stats::setNames(c(label, rep(NA, ncol(df) - 1)),
                                                             names(df))), df)
    df <- dplyr::rename(df, Variable = 1)
  } else if (label.title) {
    df <- dplyr::rename_with(df, ~ label, .cols = 1)
  }
  return(df)
}
