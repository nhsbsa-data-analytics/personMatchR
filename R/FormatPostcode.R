#' Format the postcode strings prior to matchin
#'
#' Formatting includes conversion to upper case and removal of
#' non alphanumeric characters
#'
#' @param strPostcode A string field to be cleansed
#'
#' @return A cleansed string
#' @export
#'
#' @examples
#' format_postcode(postcode)
format_postcode <- function(postcode) {

  #handle missing variables
  if(is.na(postcode)) {return(postcode)}

  # remove non alphanumeric characters and convert to upper case
  postcode <- toupper(stringr::str_replace_all(postcode, "[^[:alnum:]]", ""))

  # handle known common input errors (0 & O / I & 1 / L & 1 / 5 & S )
  # valid UK postcodes will be between 5-7 characters and
  # following set patterns on letter and number characters

  # 7 digit postcode format: AA9*9AA
  if(nchar(postcode) == 7) {
    postcode = fix_homoglyph(postcode, 1, "character")
    postcode = fix_homoglyph(postcode, 2, "character")
    postcode = fix_homoglyph(postcode, 3, "number")
    postcode = fix_homoglyph(postcode, 4, "character")
    postcode = fix_homoglyph(postcode, 6, "character")
  }

  # 6 digit postcode format: A**9AA
  if(nchar(postcode) == 6) {
    postcode = fix_homoglyph(postcode, 1, "character")
    postcode = fix_homoglyph(postcode, 4, "number")
    postcode = fix_homoglyph(postcode, 5, "character")
    postcode = fix_homoglyph(postcode, 6, "character")
  }

  # 5 digit postcode format: A99AA
  if(nchar(postcode) == 5) {
    postcode = fix_homoglyph(postcode, 1, "character")
    postcode = fix_homoglyph(postcode, 2, "number")
    postcode = fix_homoglyph(postcode, 3, "number")
    postcode = fix_homoglyph(postcode, 4, "character")
    postcode = fix_homoglyph(postcode, 5, "character")
  }
}
