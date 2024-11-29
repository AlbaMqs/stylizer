#' Add a Note to a Flextable Footer
#'
#' This function adds a numbered note to the footer of a flextable. The note is formatted with a superscript number
#' (to reference the note in the main table) followed by the provided text.
#'
#' @param ft A flextable object to which the note will be added.
#' @param txt The text of the note to add to the footer.
#' @param n The index of the note, which will appear as a superscript before the text.
#'
#' @return A flextable object with the added note in the footer.
#' @export
#'
#' @examples
#' # Create a simple flextable
#' library(flextable)
#' ft <- flextable(head(mtcars))
#'
#' # Add a note to the footer
#' ft <- st_add_note(ft, txt = "This is a sample note.", n = 1)
#'
#' # Display the flextable
#' ft
st_add_note <- function(ft, txt, n) {
  ft <- add_footer_lines(ft, values = "")
  last_footer_index <- ft$footer$content[["nrow"]]

  ft <- flextable::compose(
    x = ft,
    part = "footer",
    i = last_footer_index,
    value = as_paragraph(as_sup(as_integer(n)), txt)
  )

  return(ft)
}
