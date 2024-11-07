#' Calculate P-value for Contingency Table
#'
#' Calculates the p-value using the selected statistical test (`chi2` or `fisher`).
#'
#' @param df A contingency table in wide format.
#' @param test The statistical test to use ("chi2" or "fisher").
#' @import dplyr
#' @import stats
#' @importFrom tibble column_to_rownames
#'
#' @return A dataframe with the p-value added as a column.
#' @keywords internal
tab_utils_pval <- function(df, test) {
  if (test == "chi2") {
    tab_to_test <- df |>
      tibble::column_to_rownames(var = names(df[1])) |>
      dplyr::filter(!dplyr::if_any(dplyr::where(is.numeric), ~ . < 5))
    chisq <- stats::chisq.test(tab_to_test, correct = TRUE)
    p_value <- format_p_value(chisq$p.value)
  } else if (test == "fisher") {
    tab_to_test <- df |>
      tibble::column_to_rownames(var = names(df[1])) |>
      dplyr::filter(!dplyr::if_any(dplyr::where(is.numeric), ~ . < 5))
    fisher <- stats::fisher.test(tab_to_test)
    p_value <- format_p_value(fisher$p.value)
  }
  df <- df |>
    dplyr::mutate(p_value = c(p_value, rep(NA, nrow(df) - 1)))

  df <- df |>
    dplyr::mutate(p_value = ifelse(dplyr::if_any(dplyr::where(is.numeric), ~ . < 5),
                                   "\u2014",
                                   p_value))
  return(df)
}

#' Format P-value
#'
#' Helper function to format p-value.
#'
#' @param p The p-value to format.
#'
#' @return A formatted p-value.
#' @keywords internal
format_p_value <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "< 0.001",
    p < 0.01 ~ "< 0.01",
    TRUE ~ as.character(format(round(p, 2), nsmall = 2))
  )
}
