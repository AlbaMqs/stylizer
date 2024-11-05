#' Handle Missing Values in Contingency Table
#'
#' Adjusts for missing values in `x` based on the `na.use` parameter.
#'
#' @param df A dataframe with categorical data.
#' @param x The row variable.
#' @param na.use Mode of displaying missing values:
#'   - `"as.cat"`: As a regular category.
#'   - `"as.note"`: Count missing values as a note.
#' @param lang Table language (`"fr"` or `"en"`)
#'
#' @return A list with a dataframe (possibly with a missing category) and a missing count.
#' @keywords internal
handle_missing_values <- function(df, x, na.use, lang) {
  if (na.use == "as.note") {
    na_count <- df |>
      dplyr::filter(is.na(!!rlang::enquo(x))) |>
      nrow()
  } else if (na.use == "as.cat") {
    na_label <- dplyr::case_when(grepl("[Ff][Rr]", lang) ~ "Manquant",
                                 grepl("[Ee][Nn]", lang) ~ "Missing")
    df <- df |>
      dplyr::mutate(!!rlang::enquo(x) := forcats::fct_na_value_to_level(!!rlang::enquo(x), level = na_label))
    na_count <- NULL
  } else {
    na_count <- NULL
  }
  df <- df |>
    dplyr::filter(!is.na(!!rlang::enquo(x)))

  list(df = df, na_count = na_count)
}
