#' tab_utils_yn_title
#'
#' Generates and applies the title for a contingency table, using the label of the variable if it exists.
#' If there are missing values and \code{na.use} is set to "as.note", it appends a missing count note.
#' It also handles inline title placement or column name replacement.
#'
#' @param df The dataframe containing the contingency table.
#' @param df_init The initial dataframe.
#' @param x The variable for which to generate a title.
#' @param label.title Logical; if \code{TRUE}, replace the row variable title with its label if available.
#' @param na_count A string indicating the missing value count to append to the title.
#' @param inline.title Logical; if \code{TRUE}, places the title inline within the table instead of as the column name.
#'
#' @return The dataframe with the title appropriately set as the column name or inline.
#' @importFrom labelled var_label
#' @importFrom dplyr mutate
#' @keywords internal
tab_utils_yn_title <- function(df, df_init, x, label.title = TRUE, na_count = NULL, inline.title = FALSE) {
  # Retrieve the label if `label.title` is TRUE, else use variable name
  title <- if (label.title) {
    labelled::var_label(df_init[[rlang::as_name(rlang::enquo(x))]]) %||% rlang::as_name(rlang::enquo(x))
  } else {
    rlang::as_name(rlang::enquo(x))
  }

  # Append missing values note if `na_count` is provided
  if (!is.null(na_count) & !is.na(na_count)) {
    title <- paste0(title, "<missing:", na_count, ">")
  }

  # Apply title inline or as column name
  if (inline.title) {
    df <- df |>
      dplyr::mutate(across(1, as.character))
    df[1, 1] <- title
    names(df)[1] <- "Variable"
  } else {
    names(df)[1] <- title
  }

  return(df)
}
