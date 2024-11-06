#' Handle Missing Values for Grouped Table
#'
#' Adjusts columns and row names in the dataframe list to handle missing values.
#'
#' @param df_list A list of dataframes.
#' @param na.use Mode for displaying missing values.
#' @param inline.title Add the row variable title inline.
#'
#' @import rlang
#' @import dplyr
#' @importFrom stringr str_remove_all str_extract
#'
#' @return A list of dataframes with adjusted missing values display.
#' @keywords internal

tab_utils_handle_na_group <- function(df_list, na.use, inline.title) {

  # Check if the missing values should be displayed as a note
  if (na.use == "as.note") {

    # If the row variable title should be added inline (within the table rows)
    if (inline.title) {

      # Extract the "Variable" column from each dataframe in the list
      na_vec <- lapply(df_list, dplyr::pull, "Variable")

      # Remove the placeholder "<missing:X>" text from the "Variable" column
      df_list <- lapply(df_list, function(df) {
        dplyr::mutate(df, Variable = stringr::str_remove_all(.data$Variable, "<missing:\\d+>"))
      })

    } else {

      # If inline title is not used, extract the names of columns in each dataframe
      na_vec <- lapply(df_list, names)

      # Remove "<missing:X>" from the first column name (assumed to be the row variable name)
      df_list <- lapply(df_list, function(df) {
        colname <- names(df)[1]  # Get the name of the first column
        new_colname <- stringr::str_remove_all(colname, "<missing:\\d+>")
        names(df)[1] <- new_colname  # Replace the original column name
        df
      })
    }

    # Calculate the total count of missing values by extracting and summing up the "<missing:X>" counts
    na_count <- na_vec |>
      lapply(head, 1) |>       # Take only the first element of each column/variable vector
      unlist() |>              # Flatten the list into a vector
      stringr::str_extract("\\d+") |> # Extract numeric part indicating the missing count
      as.numeric() |>          # Convert extracted values to numeric
      sum(na.rm = TRUE)        # Sum all counts, ignoring any NA values

    # Add missing value information either inline within rows or in the column name
    if (inline.title) {

      # Add missing count information as a note to the "Variable" column inline in the first row
      df_list <- lapply(df_list, function(df) {
        dplyr::mutate(df, Variable = ifelse(dplyr::row_number() == 1,
                                            paste0(.data$Variable, "<missing:", na_count, '>'),
                                            .data$Variable))
      })

    } else {

      # Append missing count information directly to the first column name
      df_list <- lapply(df_list, function(df) {
        colname <- names(df)[1]  # Get the name of the first column
        new_colname <- paste0(colname, "<missing:", na_count, '>') # Append missing info
        names(df)[1] <- new_colname  # Update the column name
        df
      })
    }
  }

  # Return the modified list of dataframes
  return(df_list)
}
