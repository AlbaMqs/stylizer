#' tab_contingency_binded
#'
#' Bind multiple contingency tables with or without grouped columns
#'
#' @param df A dataframe containing categorical data.
#' @param ... One or more row variable.
#' @param y The column variable.
#' @param p_group An integer vector representing the width of each column group.
#' @param n.col Display count per column.
#' @param na.use Mode of displaying missing values:
#'   - "as.cat": As a regular category.
#'   - "as.note": As a note readable by st_stylize.
#'   - "no": Do not display missing values.
#' @param lang Table language ("fr" or "en")
#' @param test Statistical test used:
#'   - "chi2": Chi-square test
#'   - "fisher": Fisher's exact test
#'   - "none": No test
#' @param out Output type:
#'   - "obf": Percentage with obfuscation for small values
#'   - "pct": Standard percentage
#'   - "n": Exact number
#' @param label.title Replace the row variable title with its label
#'
#' @return A proportion dataframe
#' @import rlang
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
#'
#' @examples
#' warpbreaks$breaks <- warpbreaks$breaks > 20
#' # Example 1: Bind contingency tables without column grouping
#' tab_contingency_binded(warpbreaks, wool, tension, y = breaks, test = "chi2")
#'
#' # Example 2: Bind contingency tables with grouped columns (grouping sizes 2 and 1)
#' tab_contingency_binded(warpbreaks, wool, breaks, y = tension, p_group = c(2, 1), na.use = "as.note")
#'
#' # Note: For additional parameter information, see ?tab_contingency or ?tab_contingency_grouped.

tab_contingency_binded <- function(df, ..., y, p_group = NULL,
                                   n.col = TRUE,
                                   na.use = "as.note",
                                   test = "chi2",
                                   out = "obf",
                                   label.title = TRUE,
                                   lang = Sys.getlocale("LC_CTYPE")) {
  # Capturer les variables
  vars <- rlang::quos(...)

  # Appliquer la fonction de contingence appropriée
  if (is.null(p_group)) {
    list_tables <- purrr::map(vars, ~ tab_contingency(df, !!.x, {{y}}, n.col, na.use, test, out,
                                                      inline.title = TRUE, label.title, lang))
  } else {
    list_tables <- purrr::map(vars, ~ tab_contingency_grouped(df, !!.x, {{y}}, p_group = p_group,
                                                              n.col, na.use, test, out,
                                                              inline.title = TRUE, label.title, lang))
  }

  # Combiner les résultats
  result <- dplyr::bind_rows(list_tables)

  return(result)
}
