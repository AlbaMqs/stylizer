#' tab_contingency_yn
#'
#' Generates a contingency table for a binary variable, filtering to focus on a specified "positive" value, with optional column grouping.
#'
#' @param df A dataframe containing categorical data.
#' @param x The binary row variable to focus on (e.g., yes/no, true/false).
#' @param y The column variable.
#' @param p_group An integer vector representing the width of each column group. If \code{NULL}, columns are not grouped.
#' @param pos.value The value of \code{x} to retain in the final table (e.g., \code{TRUE} for focusing on positive cases).
#' @param n.col Display count per column.
#' @param na.use Mode of displaying missing values:
#'   - "as.cat": As a regular category.
#'   - "as.note": As a note readable by stylization functions.
#'   - "no": Do not display missing values.
#' @param test Statistical test used:
#'   - "chi2": Chi-square test.
#'   - "fisher": Fisher's exact test.
#'   - "none": No test.
#' @param out Output type:
#'   - "obf": Percentage with obfuscation for small values.
#'   - "pct": Standard percentage.
#'   - "n": Exact number.
#' @param inline.title Add the row variable title inline in the table.
#' @param label.title Replace the row variable title with its label if a label exists.
#' @param lang Table language ("fr" or "en").
#'
#' @return A dataframe containing the filtered contingency table for the specified "positive" value of \code{x}.
#' @import rlang
#' @importFrom dplyr mutate filter across first
#' @importFrom stringr str_extract str_remove_all
#' @importFrom labelled var_label
#' @export
#'
#' @examples
#' # Example 1: Basic usage without column grouping, focusing on TRUE values
#' tab_contingency_yn(warpbreaks, wool, tension, pos.value = "A")
#'
#' # Example 2: With grouped columns, focusing on a positive value with Chi-square test
#' tab_contingency_yn(warpbreaks, wool, tension, p_group = c(2, 1), pos.value = "A", test = "chi2")
#'
#' # Note: Refer to ?tab_contingency and ?tab_contingency_grouped for further parameter details.

tab_contingency_yn <- function(df, x, y, p_group = NULL, pos.value = TRUE,
                        n.col = TRUE,
                        na.use = "as.note",
                        test = "chi2",
                        out = "obf",
                        inline.title = FALSE,
                        label.title = TRUE,
                        lang = Sys.getlocale("LC_CTYPE")){
  df_init <- df
  # Step 1: Get missing
  if(na.use == "as.note"){
    na_count <- df |>
      filter(is.na({{y}})) |>
      nrow()
  }
  else{
    na_count <- NA
  }

  # Step 2: Classic contingence table
  if(is.null(p_group)){
    df <- df |>
      mutate(!!enquo(x) := as.character(!!enquo(x))) |>
      tab_contingency({{x}}, {{y}}, n.col = n.col, na.use = "no", test = test, out = out, inline.title=F, label.title=F, lang = lang)
  }
  else{
    df <- df |>
      mutate(!!enquo(x) := as.character(!!enquo(x))) |>
      tab_contingency_grouped({{x}}, {{y}}, p_group, n.col = n.col, na.use = "no", test = test, out = out, inline.title=F, label.title=F, lang = lang)
  }

  # Step 3: Get p value
  if(test != "none"){
    df <- df |>
      dplyr::mutate(dplyr::across(tidyselect::matches("p_value"),
                                  ~ dplyr::first(.)))
  }

  # Step 4: Filter positive value
  df <- df |>
    filter(!!enquo(x) == pos.value)

  # Step 5: Add title
  df <- tab_utils_yn_title(df, df_init, x = {{x}}, label.title = label.title, na_count = na_count, inline.title = inline.title)

  return(df)
}
