#' Stylize a Flextable for Improved Readability and Structure
#'
#' Formats and enhances the layout of a flextable by structuring content, formatting cells, and adding footnotes
#' as needed for optimal readability. This function can handle missing data, non-additive categories, and other
#' specific notes in multiple languages.
#'
#' @param data A table or dataframe to be formatted.
#' @param theme The flextable theme to apply. Default is `"classy"`.
#' @param group Grouping level indicator. Leave `NULL` for single-level grouping; set to `2` for multi-level.
#' @param lang Language for table content and footnotes. Accepts `"en"` for English or `"fr"` for French.
#'
#' @return A stylized flextable object, ready for printing or exporting to Word.
#' @import flextable
#' @importFrom stringr str_extract str_to_lower
#' @export
#'
#' @examples
#' # Example of using the st_stylize function
#'
#' # Example 1: Applying st_stylize to a contingency table
#' tab_contingency_binded(warpbreaks, wool, breaks, y = tension, p_group = c(2, 1), na.use = "as.note") |>
#' st_stylize()
#'
#' # Example 2: Applying st_stylize with a custom note and grouped p-values
#' warpbreaks |>
#' dplyr::rename(`wool<note:A simple note>` = wool) |>
#' tab_contingency_yn_binded(breaks, `wool<note:A simple note>`, y = tension, p_group = c(2,1),
#'                           title = "Combined Wool and Tension Table", pos.value = "A") |>
#' st_stylize(lang = "fr")
st_stylize <- function(data,
                       theme = "classy",
                       group = NULL,
                       lang = Sys.getlocale("LC_CTYPE")){

  if(!is.null(group)){
    liste_ft <- st_utils_grouped(data, group)
    data <- liste_ft[[1]]
    ft <- liste_ft[[2]]
  }
  else{
    ft <- flextable(data)
  }

  n <- as.integer(1)
  lang <- stringr::str_extract(lang, "([Ff][Rr]|[Ee][Nn])") |>
    stringr::str_to_lower()

  # Affichage des missing
  ft <- st_utils_add_missing(ft, data, lang)

  # Mise en forme des colonnes de p-value
  ft <- st_utils_add_pvalue_note(ft, data, lang, n, group = group)
  n <- ifelse(any(grepl("p_value", names(data))),
              as.integer(n + 1),
              as.integer(n))

  # Ajout des non addible
  list_ft <- st_utils_add_nonadd_note(ft, data, lang, n)
  n <- list_ft[[1]]
  ft <- list_ft[[2]]

  # Ajout des notes de bas de page personnalisées
  list_ft <- st_utils_add_custom_note(ft, data, n)
  n <- list_ft[[1]]
  ft <- list_ft[[2]]

  # Mise en page de la colonne de modalité
  mod <- is.na(data[[2]])
  for (i in which(mod)) {
    ft <- flextable::merge_at(ft, i = i, part = "body")
  }
  ft <- flextable::padding(ft, i = !mod, padding.left = 20, part = "body")
  ft <- flextable::bold(ft, i = mod, bold = TRUE)
  ft <- flextable::compose(ft, part = "header", i = 1, j = 1, value = flextable::as_paragraph(""))

  # Application du thème
  if (theme == "classy") {
    ft <- stylizer::theme_classy(ft)
  }

  return(ft)
}
