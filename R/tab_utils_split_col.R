#' Split Columns into Groups
#'
#' This function divides a vector of columns into specified group sizes.
#'
#' @param cols A vector of column names to be split.
#' @param p_group An integer vector specifying the size of each group.
#'
#' @return A list of column name groups.
#' @export
tab_utils_split_col <- function(cols, p_group) {
  start <- 1
  result <- list()

  for (i in seq_along(p_group)) {
    end <- start + p_group[i] - 1
    result[[i]] <- cols[start:end]
    start <- end + 1
  }

  return(result)
}
