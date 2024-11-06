#' Display Counts per Column
#'
#' Adds count information for each level of `y` if `n.col` is `TRUE`.
#'
#' @param df A dataframe with categorical data.
#' @param y The column variable.
#' @param n.col Logical, if `TRUE`, display counts per column.
#' @import dplyr
#' @importFrom rlang enquo as_name
#'
#' @return A dataframe with counts per column level in `y`.
#' @keywords internal
tab_utils_display_ncol <- function(df, df_init, y) {
  # Capturer l'expression de `y`
  y <- rlang::enquo(y)

  # Calculer les fréquences dans `df_init` et créer la colonne `name`
  name <- df_init |>
    dplyr::count(!!y, name = 'name') |>
    dplyr::filter(!is.na(!!y)) |>
    dplyr::mutate(name = paste0(!!y, "\nN=", name),
                  !!y := as.factor(!!y))

  # Effectuer une jointure pour remplacer la colonne `y` dans `df`
  df <- df |>
    dplyr::left_join(name, by = rlang::as_name(y)) |>
    dplyr::mutate(!!y := name)

  return(df)
}
