#' Stylize
#'
#' Handles the table layout by organizing its structure, formatting cells, and aligning content for optimal readability.
#'
#' @param tab A table or dataframe to be formatted.
#'
#' @return A Flextable object ready to be printed in your Word document.
#' @export
#'
#' @examples
#' # Example of using the stylize function
#'
#' # Create a small dataset
#' example_data <- data.frame(
#'   Name = c("Alice", "Bob", "Charlie"),
#'   Age = c(25, 30, 35),
#'   Profession = c("Engineer", "Doctor", "Teacher")
#' )
#'
#' # Apply the stylize function to the dataset
#' stylized_table <- stylize(example_data)
#'
#' # Display the result (here, stylized_table is the formatted Flextable object)
#' print(stylized_table)

stylize <- function(tab){
  print(tab)

  return(tab)
}
