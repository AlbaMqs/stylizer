#' tab_contingency_yn_binded
#'
#' Combines multiple contingency tables focused on a specific "positive" value, with optional column grouping, into a single table.
#'
#' @param df A dataframe containing categorical data.
#' @param ... One or more binary row variables to include in the combined table.
#' @param y The column variable.
#' @param p_group An integer vector representing the width of each column group. If \code{NULL}, columns are not grouped.
#' @param title A title row added at the top of the final combined table.
#' @param pos.value Value of ... to consider as the positive value.
#' @param n.col Display count per column.
#' @param na.use Mode of displaying missing values:
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
#' @param label.title Replace the row variable title with its label if a label exists.
#' @param lang Table language ("fr" or "en").
#'
#' @return A dataframe containing the combined contingency tables for the specified "positive" values of each binary variable in \code{...}.
#' @import rlang
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom stringr str_extract str_remove
#' @export
#'
#' @examples
#' warpbreaks$breaks <- ifelse(warpbreaks$breaks > 20, "A", "B")
#' # Example 1: Combine contingency tables without column grouping
#' tab_contingency_yn_binded(warpbreaks, breaks, wool, y = tension,
#'                           title = "Combined Wool and Tension Table", pos.value = "A")
#'
#' # Example 2: Combine contingency tables with grouped columns
#' tab_contingency_yn_binded(warpbreaks, breaks, wool, y = tension, p_group = c(2,1),
#'                           title = "Combined Wool and Tension Table", pos.value = "A")
#'
#' # Note: Refer to ?tab_prop_yn for additional parameter details.

tab_contingency_yn_binded <- function(df, ..., y, p_group = NULL, title, pos.value = TRUE,
                             n.col = TRUE,
                             na.use = "as.note",
                             test = "chi2",
                             out = "obf",
                             label.title = TRUE,
                             lang = Sys.getlocale("LC_CTYPE")){
  vars <- quos(...)
  # Step 1: Create contingency tables
  list_tables <- map(vars, ~ tab_contingency_yn(df, !!.x, {{y}}, p_group = p_group, pos.value,
                                         n.col, na.use, test, out,
                                         inline.title = TRUE, label.title, lang))

  # Step 2: Bind all table to get one bigger
  result <- bind_rows(list_tables)

  # Step 3: Handle missing value
  if(na.use == "as.note"){
    na_count <- result$Variable |>
      stringr:::str_extract("(?<=missing\\:)\\d+") |>
      as.numeric() |>
      sum()

    title <- paste0(title, "<missing:", na_count, ">")

    result$Variable <- result$Variable |>
      stringr:::str_remove("<missing\\:\\d+>")
  }

  title <- paste0(title, "<nonadd>")

  result <- bind_rows(tibble(!!!setNames(c(title, rep(NA, ncol(result) - 1)),
                                         names(result))), result)

  return(result)
}
