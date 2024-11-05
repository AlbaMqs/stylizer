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
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom stats chisq.test fisher.test setNames
#' @importFrom forcats fct_na_value_to_level
#' @importFrom labelled var_label
#' @importFrom rlang enquo as_name
#' @export
#'
#' @examples
#' warpbreaks |>
#' contingency_tab(wool, tension)

contingency_tab <- function(df, x, y,
                            n.col = TRUE,
                            na.use = "as.note",
                            test = "chi2",
                            out = "obf",
                            inline.title = FALSE,
                            label.title = TRUE,
                            lang = Sys.getlocale("LC_CTYPE")) {
  # Ensure data is properly prepared
  df_init <- as.data.frame(df)

  df <- df |>
    dplyr::mutate(!!rlang::enquo(x) := as.factor(!!rlang::enquo(x)),
                  !!rlang::enquo(y) := as.factor(!!rlang::enquo(y))) |>
    dplyr::filter(!is.na(!!rlang::enquo(y)))

  # NA handling system
  if (na.use == "as.note") {
    na <- df |>
      dplyr::filter(is.na(!!rlang::enquo(x))) |>
      nrow()
  } else if (na.use == "as.cat") {
    na.name <- dplyr::case_when(grepl("[Ff][Rr]", lang) ~ "Manquant",
                                grepl("[Ee][Nn]", lang) ~ "Missing")
    df <- df |>
      dplyr::mutate(!!rlang::enquo(x) := forcats::fct_na_value_to_level(!!rlang::enquo(x), level = na.name))
  }

  df <- df |>
    dplyr::filter(!is.na(!!rlang::enquo(x)))

  # Display counts per column
  if (n.col) {
    df <- df |>
      dplyr::group_by(!!rlang::enquo(y)) |>
      dplyr::mutate(!!rlang::enquo(y) := paste0(!!rlang::enquo(y), "\nN=", dplyr::n())) |>
      dplyr::ungroup()
  }

  # Contingency table
  df <- df |>
    dplyr::count(!!rlang::enquo(x), !!rlang::enquo(y), .drop = FALSE) |>
    tidyr::pivot_wider(
      names_from = rlang::as_name(rlang::enquo(y)),
      values_from = n,
      values_fill = 0
    )

  # P-value calculation
  if (test == "chi2") {
    tab_to_test <- df |>
      tibble::column_to_rownames(var = names(df[1])) |>
      dplyr::filter(!dplyr::if_any(dplyr::where(is.numeric), ~ . < 5))

    chisq <- stats::chisq.test(tab_to_test, correct = TRUE)

    p.value <- dplyr::case_when(
      chisq$p.value < 0.001 ~ "< 0.001",
      chisq$p.value < 0.01 ~ "< 0.01",
      TRUE ~ as.character(format(round(chisq$p.value, 2), nsmall = 2))
    )

    df <- df |>
      dplyr::mutate(p_value = c(p.value, rep(NA, nrow(df) - 1))) |>
      dplyr::mutate(p_value = ifelse(dplyr::if_any(dplyr::where(is.numeric), ~ . < 5),
                                     "\u2014",
                                     p_value))
  } else if (test == "fisher") {
    tab_to_test <- df |>
      tibble::column_to_rownames(var = names(df[1])) |>
      dplyr::filter(!dplyr::if_any(dplyr::where(is.numeric), ~ . < 5))

    fisher <- stats::fisher.test(tab_to_test)

    p.value <- dplyr::case_when(
      fisher$p.value < 0.001 ~ "< 0.001",
      fisher$p.value < 0.01 ~ "< 0.01",
      TRUE ~ as.character(format(round(fisher$p.value, 2), nsmall = 2))
    )

    df <- df |>
      dplyr::mutate(p_value = c(p.value, rep(NA, nrow(df) - 1))) |>
      dplyr::mutate(p_value = ifelse(dplyr::if_any(dplyr::where(is.numeric), ~ . < 5),
                                     "\u2014",
                                     p_value))
  }

  # Percentage calculation
  pourcentage <- function(n, eff) {
    percent <- (n / sum(eff)) * 100
    percent <- formatC(percent, format = "f", digits = 2)
    percent <- paste(percent, "%")
    return(percent)
  }

  if (out == "obf") {
    df <- df |>
      dplyr::mutate(across(
        dplyr::where(is.numeric),
        ~ dplyr::case_when(.x >= 5 ~ pourcentage(.x, sum(.x)), .x > 0 ~ "(<5)", .x == 0 ~ "(0)")
      ))
  } else if (out == "pct") {
    df <- df |>
      dplyr::mutate(across(
        dplyr::where(is.numeric),
        ~ pourcentage(.x, sum(.x))
      ))
  }

  # Adding a title
  label <- labelled::var_label(df_init[[rlang::as_name(rlang::enquo(x))]])
  if (is.null(label)) {
    label <- rlang::as_name(rlang::enquo(x))  # Si pas de label, utilise le nom de la variable
  }
  label <- ifelse(na.use == "as.note", paste0(label, "<missing:", na, ">"), label)

  if (inline.title) {
    df <- dplyr::bind_rows(tibble::tibble(!!!stats::setNames(c(label, rep(NA, ncol(df) - 1)),
                                                             names(df))), df)
    df <- dplyr::rename(df, Variable = 1)
  } else if (label.title) {
    df <- dplyr::rename_with(df, ~ label, .cols = 1)
  }

  return(df)
}
