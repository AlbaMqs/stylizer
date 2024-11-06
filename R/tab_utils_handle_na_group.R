#' Handle Missing Values for Grouped Table
#'
#' Adjusts columns and row names in the dataframe list to handle missing values.
#'
#' @param df_list A list of dataframes.
#' @param na.use Mode for displaying missing values.
#' @param inline.title Add the row variable title inline.
#'
#' @import rlang
#' @importFrom dplyr pull filter mutate rename
#' @importFrom stringr str_remove_all str_extract
#'
#' @return A list of dataframes with adjusted missing values display.
#' @keywords internal

tab_utils_handle_na_group <- function(df_list, na.use, inline.title) {
  if (na.use == "as.note") {
    if (inline.title) {
      na_vec <- lapply(df_list, dplyr::pull, "Variable")

      df_list <- lapply(df_list, function(df) {
        dplyr::mutate(df, Variable = stringr::str_remove_all(Variable, "<missing:\\d+>"))
      })
    } else {
      na_vec <- lapply(df_list, names)

      df_list <- lapply(df_list, function(df) {
        colname <- names(df)[1]
        new_colname <- stringr::str_remove_all(colname, "<missing:\\d+>")
        names(df)[1] <- new_colname
        df
      })
    }

    na_count <- na_vec |>
      lapply(head, 1) |>
      unlist() |>
      stringr::str_extract("\\d+") |>
      as.numeric() |>
      sum(na.rm = TRUE)

    if (inline.title) {
      df_list <- lapply(df_list, function(df) {
        dplyr::mutate(df, Variable = ifelse(dplyr::row_number() == 1, paste0(Variable, "<missing:", na_count, '>'), Variable))
      })
    } else {
      df_list <- lapply(df_list, function(df) {
        colname <- names(df)[1]
        new_colname <- paste0(colname, "<missing:", na_count, '>')
        names(df)[1] <- new_colname
        df
      })
    }
  }

  return(df_list)
}
