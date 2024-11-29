#' Add Missing Data Information to Flextable with Language Support
#'
#' This internal utility function detects and processes `<missing:X>` tags in the `Variable` column of `data`,
#' removing the tags from the main content and appending a summary note to the flextable footer, in the specified language.
#'
#' @param ft A flextable object to which missing data information will be added.
#' @param data A dataframe containing a `Variable` column with `<missing:X>` tags for missing data counts.
#' @param lang Language for the footer text. Accepts `"en"` for English or `"fr"` for French.
#'
#' @return A flextable object with a summary note added in the footer.
#' @keywords internal
#'
#' @details
#' - The function detects `<missing:X>` tags in the `Variable` column of `data`, where `X` is the count of missing data.
#' - These tags are removed from the main content, and a summary of missing data is generated.
#' - The summary note, detailing missing counts for each variable or for multiple groups, is appended to the footer of `ft` in the specified language.
#'
#' @examples
#' # (For internal use only; no example provided.)
st_utils_add_missing <- function(ft, data, lang = Sys.getlocale("LC_CTYPE")) {

  lang <- stringr::str_extract(lang, "([Ff][Rr]|[Ee][Nn])") |>
    stringr::str_to_lower()

  # Vérifier que lang est soit "en" soit "fr"
  if (!lang %in% c("en", "fr")) {
    stop("Invalid language specified. Use 'en' for English or 'fr' for French.")
  }

  # Messages en fonction de la langue
  total_missings_msg <- if (lang == "en") {
    "Number of total missings for each variable:"
  } else {
    "Nombre total de valeurs manquantes pour chaque variable :"
  }

  multi_populations_msg <- if (lang == "en") {
    "Number of total missings for the"
  } else {
    "Nombre total de valeurs manquantes pour les"
  }

  populations_text <- if (lang == "en") {
    "populations"
  } else {
    "populations"
  }

  with_each_variable_msg <- if (lang == "en") {
    "for each variable:"
  } else {
    "pour chaque variable :"
  }
  on <- if (lang == "en") {
    " on "
  } else {
    " pour "
  }

  # Détection des <missing>
  row.var <- grep("<missing\\:\\d+>", data$Variable)
  if(length(row.var) == 0){
    return(ft)
  }
  missing.list <- ""

  # Suppression des missings dans le corps
  for (row in row.var) {
    missing <- data$Variable[row] |>
      str_extract("(?<=<missing\\:)\\d+(?=>)")

    var_name <-  data$Variable[row] |>
      str_remove_all("<.+>")

    missing <- paste0("; n=", missing, on, str_to_lower(var_name))
    missing.list <- paste0(missing.list, missing)

    data$Variable[row] <- data$Variable[row] |>
      str_remove("<missing\\:\\d+>")

    ft <- flextable::compose(ft,
                             i = row,
                             j = "Variable",
                             value = as_paragraph(as_b(data$Variable[row])))
  }

  missing.list <- str_remove(missing.list, "^;")

  # Nombre de poulations différentes
  ncol_data <- data |>
    select(-contains("p_value")) |>
    select(-1) |>
    ncol()

  # Population totale
  n_ens <- data |>
    select(-contains("p_value")) |>
    names() |>
    str_extract_all("(?<=N=)\\d+")  |>
    as.numeric() |>
    sum(na.rm = TRUE)

  # Concatenation de la note de bas de page
  if (ncol_data == 1) {
    missing.list <- paste0(total_missings_msg, missing.list)
  } else {
    missing.list <- paste0(
      multi_populations_msg, " ",
      ncol_data, " ", populations_text,
      " (n=", n_ens, ") ",
      with_each_variable_msg, missing.list
    )
  }

  # Ajout de la note de bas de page
  ft <- add_footer_lines(ft, values = missing.list)

  return(ft)
}
