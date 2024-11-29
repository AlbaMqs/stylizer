#' Add Custom Notes to Flextable
#'
#' Detects `<note:...>` tags in the `Variable` column of `data` and adds a custom footnote to the flextable.
#'
#' @param ft A flextable object to which the note will be added.
#' @param data A dataframe containing `<note:...>` tags in the `Variable` column.
#' @param n Index number for the footnote.
#'
#' @return A flextable object with the custom notes added.
#' @keywords internal
#' @importFrom stringr str_extract str_remove
st_utils_add_custom_note <- function(ft, data, n) {
  row.var <- grep("<note\\:.+>", data$Variable)
  for (row in row.var) {
    data$Variable[row] <- stringr::str_remove(data$Variable[row], "<missing\\:\\d+>")
    note_content <- stringr::str_extract(data$Variable[row], "(?<=<note\\:).+(?=>)")
    data$Variable[row] <- stringr::str_remove(data$Variable[row], "<note\\:.+>")
    ft <- flextable::compose(ft, i = row, j = "Variable", value = flextable::as_paragraph(data$Variable[row], flextable::as_sup(n)))
    ft <- st_add_note(ft, note_content, n)
    n <- as.integer(n + 1)
  }
  liste <- list(n, ft)
  return(liste)
}
