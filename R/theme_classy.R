#' Classy Theme
#'
#' Applies a refined and professional style to a `flextable` object, formatting the table headers, body, and footer for improved readability and visual appeal.
#'
#' @param ft A `flextable` object that you want to style.
#'
#' @return A `flextable` object with the applied classy theme formatting.
#' @import flextable
#' @import officer
#' @export
#'
#' @examples
#' # Example usage of classy_theme*
#' library(flextable)
#' library(officer)
#' ft <- flextable(head(iris))  # Create a simple flextable
#' styled_ft <- theme_classy(ft)  # Apply the classy theme to the flextable
#' styled_ft

theme_classy <- function(ft) {
  # Formatting the headers
  ft <- bold(ft, part = "header")  # Bold text for headers
  ft <- align(ft, align = "center", part = "header")  # Center-align header text
  ft <- fontsize(ft, size = 10.5, part = "header")  # Font size for header
  ft <- font(ft, fontname = "Times New Roman", part = "header")

  # Formatting the body of the table
  ft <- fontsize(ft, size = 10, part = "body")  # Smaller font size for the body
  ft <- font(ft, fontname = "Times New Roman", part = "body")  # Times New Roman font for the body
  ft <- align(ft, align = "center", part = "body")  # Center-align cell content
  ft <- valign(ft, valign = "center", part = "body")  # Vertical alignment to the center

  # Formatting the first column (usually for labels)
  ft <- align(ft,
              align = "left",
              part = "body",
              j = 1)  # Left-align the content of the first column

  # Formatting the footer of the table
  ft <- italic(ft, part = "footer")  # Italicize text in the footer
  ft <- fontsize(ft, size = 9, part = "footer")  # Smaller font size for the footer
  ft <- line_spacing(ft, space = 0.8, part = "footer")  # Reduce line spacing in the footer
  ft <- padding(ft,
                padding.top = 1,
                padding.bottom = 1,
                part = "footer")  # Adjust padding in the footer cells

  # Adjust borders
  thick_border <- fp_border(color = "black", width = 1.5)

  ft <- hline_top(ft, part = "header", border = thick_border)  # Thick line above the header
  ft <- hline_bottom(ft, part = "header", border = thick_border)  # Thin line below the header
  ft <- hline_bottom(ft, part = "body", border = thick_border)  # Thick line below the body of the table

  # Adjust cell padding to create more space in the table
  ft <- padding(
    ft,
    padding.top = 2,
    padding.bottom = 2,
    part = "all")

  # Set left and right padding to 0 for all columns except the first one
  num_cols <- length(ft$body$dataset)
  ft <- padding(ft,
                i = NULL,
                j = 2:num_cols,
                padding.left = 0,
                padding.right = 0)

  # Adjust column widths to fit the content
  ft <- autofit(ft)

  return(ft)
}
