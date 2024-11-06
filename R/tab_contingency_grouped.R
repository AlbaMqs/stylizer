#' tab_contingency_grouped
#'
#' Generates a contingency table with grouped columns
#'
#' @param df A dataframe containing categorical data.
#' @param x The row variable.
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
#' @param inline.title Add the row variable title inline in the table
#' @param label.title Replace the row variable title with its label
#'
#' @return A proportion dataframe
#' @import rlang
#' @import dplyr
#' @importFrom purrr map map2 reduce
#' @importFrom utils head
#' @export
#'
#' @examples
#' # Basic usage: grouped contingency table with specified column groups
#' # For more details on additional parameters, see ?tab_contingency
#'
#' # Example 1: Group columns into two groups with sizes 2 and 1
#' tab_contingency_grouped(warpbreaks, wool, tension, c(2, 1))
#'
#' # Example 2: Group columns with different group sizes, specifying missing
#' # values handling and a statistical test
#' warpbreaks_with_na <- warpbreaks
#' warpbreaks_with_na$wool[c(5, 15, 25)] <- NA
#' tab_contingency_grouped(warpbreaks_with_na, wool, tension,
#'                         c(1, 2), na.use = "as.note", test = "chi2")
#'
#' # Example 3: Group columns with percentage output and labels inline
#' tab_contingency_grouped(warpbreaks, wool, tension,
#'                         c(2, 1), out = "pct", inline.title = TRUE)
#'
#' # Example 4: Grouped table with column label replacement for `wool`
#' labelled::var_label(warpbreaks$wool) <- "Type de laine"
#' tab_contingency_grouped(warpbreaks, wool, tension,
#'                         c(1, 2), label.title = TRUE)
#'
#' # Note: Additional customization options are available.
#' # Refer to ?tab_contingency for more details on parameters such as `n.col`,
#' # `na.use`, `test`, and `out`.


tab_contingency_grouped <- function(df, x, y, p_group,
                                    n.col = TRUE,
                                    na.use = "as.note",
                                    test = "chi2",
                                    out = "obf",
                                    inline.title = FALSE,
                                    label.title = TRUE,
                                    lang = Sys.getlocale("LC_CTYPE")) {

  # Step 1: Split columns by group
  list_col <- df |>
    dplyr::pull({{y}}) |>
    levels() |>
    tab_utils_split_col(p_group)

  df_list <- purrr::map2(
    purrr::map(1:length(p_group), ~ df),
    list_col,
    ~ dplyr::filter(.x, {{y}} %in% .y)
  )

  # Step 2: Apply the contingency table function to each group
  df_list <- lapply(df_list, tab_contingency, {{x}}, {{y}}, n.col, na.use, test, out, inline.title, label.title, lang)

  # Step 3: Rename p-value columns if test is applied
  if(test != "none"){
    df_list <- lapply(seq_along(df_list), function(i) {
      df_list[[i]] |>
        dplyr::rename(!!paste0("p_value_", i) := .data$p_value)
    })
  }

  # Step 4: Handle missing values according to na.use setting
  df_list <- tab_utils_handle_na_group(df_list, na.use, inline.title)

  # Step 5: Combine dataframes into a single table
  df_combined <- purrr::reduce(df_list, dplyr::full_join, by = names(df_list[[1]])[1])

  return(df_combined)

}

