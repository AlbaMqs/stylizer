#' contingency_tab
#'
#' Generates a simple contingency table.
#'
#' @param df A dataframe containing categorical data.
#' @param x The row variable.
#' @param y The column variable.
#' @param n.col Display count per column.
#' @param na.use Mode of displaying missing values:
#'   - "as.cat": As a regular category.
#'   - "as.note": As a note readable by stylize.
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
#' @param inline.title Add the row variable title inline in the table
#' @param label.title Replace the row variable title with its label
#'
#' @return A proportion dataframe
#' @import dplyr
#' @import rlang
#' @export
#'
#' @examples
#' # Example 1: Simple contingency table without advanced options
#' contingency_tab(warpbreaks, wool, tension)
#'
#' # Example 2: Treat missing values in `wool` as a separate category
#' warpbreaks_with_na <- warpbreaks
#' warpbreaks_with_na$wool[c(5, 15, 25)] <- NA  # Add some NA values for demonstration
#' contingency_tab(warpbreaks_with_na, wool, tension, na.use = "as.cat")
#'
#' # Example 3: Display missing values as a note
#' contingency_tab(warpbreaks_with_na, wool, tension, na.use = "as.note")
#'
#' # Example 4: Contingency table with chi-square test
#' contingency_tab(warpbreaks, wool, tension, test = "chi2")
#'
#' # Example 5: Contingency table with Fisher's exact test for small sample sizes
#' contingency_tab(warpbreaks, wool, tension, test = "fisher")
#'
#' # Example 6: Display percentages without obfuscation
#' contingency_tab(warpbreaks, wool, tension, out = "pct")
#'
#' # Example 7: Display percentages with obfuscation for small values
#' contingency_tab(warpbreaks, wool, tension, out = "obf")
#'
#' # Example 8: Add inline title for the `wool` variable
#' contingency_tab(warpbreaks, wool, tension, inline.title = TRUE)
#'
#' # Example 9: Use a variable label for `wool`
#' labelled::var_label(warpbreaks$wool) <- "Type de laine"
#' contingency_tab(warpbreaks, wool, tension, label.title = TRUE)
#'
#' # Example 10: Combine multiple options
#' contingency_tab(warpbreaks_with_na, wool, tension,
#'                 n.col = TRUE, na.use = "as.cat", test = "chi2", out = "pct",
#'                 inline.title = TRUE, label.title = TRUE)

contingency_tab <- function(df, x, y,
                            n.col = TRUE,
                            na.use = "as.note",
                            test = "chi2",
                            out = "obf",
                            inline.title = FALSE,
                            label.title = TRUE,
                            lang = Sys.getlocale("LC_CTYPE")) {
  # Step 1: Prepare data by converting `x` and `y` to factors and filtering NA in `y`
  df_init <- as.data.frame(df)

  df <- df |>
    dplyr::mutate(!!rlang::enquo(x) := as.factor(!!rlang::enquo(x)),
                  !!rlang::enquo(y) := as.factor(!!rlang::enquo(y))) |>
    dplyr::filter(!is.na(!!rlang::enquo(y)))

  # Step 2: Handle missing values
  na_handling <- handle_missing_values(df, !!rlang::enquo(x), na.use, lang)
  df <- na_handling$df
  na_count <- na_handling$na_count

  df <- df |>
    dplyr::filter(!is.na(!!rlang::enquo(x)))

  # Step 3: Display counts per column if specified
  df <- display_counts_per_column(df, !!rlang::enquo(y), n.col)

  # Step 4: Create contingency table
  df <- create_contingency_table(df, !!rlang::enquo(x), !!rlang::enquo(y))

  # Step 5: Calculate p-value if required
  if (test != "none") {
    df <- calculate_p_value(df, test)
  }

  # Step 6: Calculate percentages based on output type
  if (out != "n") {
    df <- calculate_percentages(df, out)
  }

  # Step 7: Add inline title if required
  df <- add_inline_title(df, !!rlang::enquo(x), df_init, inline.title, label.title, na_count)

  return(df)
}
