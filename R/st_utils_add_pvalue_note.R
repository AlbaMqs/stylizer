#' Add p-value Note and Format to Flextable
#'
#' Formats columns containing p-values and adds a footnote explaining p-value calculations, if applicable.
#'
#' @param ft A flextable object to which the p-value note will be added.
#' @param data A dataframe with columns that may contain p-values, identified by names containing "p_value".
#' @param lang Language for the note text. Accepts `"en"` for English or `"fr"` for French.
#' @param n Index number for the footnote.
#' @param group Grouping level indicator. Leave `NULL` for single-level grouping; set to `2` for multi-level.
#'
#' @return A flextable object with formatted p-value columns and the p-value note added.
#' @keywords internal
#' @importFrom flextable compose as_paragraph as_i as_sup
#' @importFrom stringr str_detect
st_utils_add_pvalue_note <- function(ft, data, lang, n, group = NULL) {
  col.var <- grep("p_value", names(data))
  if (length(col.var) == 0) return(ft)  # Return immediately if no p-value columns

  # Set the header row index depending on grouping level
  i <- if (is.null(group)) 1 else 2

  # Format p-value columns in the header
  ft <- flextable::compose(ft,
                           part = "header",
                           j = col.var,
                           i = i,
                           value = flextable::as_paragraph(flextable::as_i("p value"), flextable::as_sup(n)))

  # Define the p-value note based on language
  p_message <- ifelse(
    lang == "en",
    "Categories with counts lower than 5 are excluded from the χ² test used to calculate the p value.",
    "Les modalités dont l'effectif est inférieur à 5 sont exclues du test de χ² utilisé pour calculer la p value"
  )

  # Add the p-value note to the footer
  ft <- st_add_note(ft, p_message, n)

  return(ft)
}
