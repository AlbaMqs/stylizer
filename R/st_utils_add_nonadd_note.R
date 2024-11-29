#' Add Non-Additivity Note to Flextable
#'
#' Adds a footnote for non-additive categories in the `Variable` column.
#'
#' @param ft A flextable object to which the note will be added.
#' @param data A dataframe containing `<nonadd>` tags in the `Variable` column.
#' @param lang Language for the note text. Accepts `"en"` for English or `"fr"` for French.
#' @param n Index number for the footnote.
#'
#' @return A flextable object with the non-additivity note added.
#' @keywords internal
#' @importFrom stringr str_remove
st_utils_add_nonadd_note <- function(ft, data, lang, n) {
  row.var <- grep("<nonadd>", data$Variable)
  if(length(row.var) > 0){
    for (row in row.var) {
      data$Variable[row] <- stringr::str_remove(data$Variable[row], "<nonadd>")
      data$Variable[row] <- stringr::str_remove(data$Variable[row], "<missing\\:\\d+>")
      ft <- flextable::compose(ft, i = row, j = "Variable", value = flextable::as_paragraph(flextable::as_b(data$Variable[row]), flextable::as_sup(n)))
    }

    nonadd_message <- if (lang == "en") {
      "Categories are non-exclusive; therefore, the totals in the columns may not add up to 100%"
    } else {
      "Les modalités sont non-exclusives ; par conséquent, le total des colonnes peut ne pas valoir 100%"
    }

    ft <- st_add_note(ft, nonadd_message, n)
    n <- as.integer(n + 1)
  }
  liste <- list(n, ft)
  return(liste)
}
