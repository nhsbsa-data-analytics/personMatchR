#' FixHomoglyph
#' Fix common homoglyph errors that appear
#'
#' @param x A character to be fixed
#'
#' @return A character where known homoglyphs have been switched
#' @export
#'
#' @examples
#' FixHomoglyph(x)
FixHomoglyph <- function(x) {

  # check if character is a known homoglyph, flipping value where possible
  if (x == "0") {
    y = "O"
  } else if (x == "5") {
    y = "S"
  } else if (x == "O") {
    y = "0"
  } else if (x == "I") {
    y = "1"
  } else if (x == "L") {
    y = "1"
  } else if (x == "S") {
    y = "5"
  } else {
    y = x
  }

  # return output
  return(y)
}
