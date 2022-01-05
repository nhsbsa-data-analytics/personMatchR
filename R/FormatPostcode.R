#' FormatPostcode
#' Format the postcode strings prior to matching
#' Formatting includes conversion to upper case and removal of non alphanumeric characters
#'
#' @param strPostcode A string field to be cleansed
#'
#' @return A cleansed string
#' @export
#'
#' @examples
#' FormatName(strPostcode)
FormatPostcode <- function(strPostcode) {

  # remove non alphanumeric characters and convert to upper case
  strPostcodeFormat <- toupper(str_replace_all(strPostcode, "[^[:alnum:]]", ""))

  # handle known common input errors (0 & O / I & 1 / L & 1 / 5 & S )
  # valid UK postcodes will be between 5-7 characters and following set patterns on letter and number characters

  # 7 digit postcode format: AA9*9AA
  if(nchar(strPostcodeFormat) == 7) {
    # check letter positions
    for(i in c(1,2,6,7)){
      x <- substr(strPostcodeFormat,i,i)
      if (str_detect(x,"[^[:alpha:]]")){
        substr(strPostcodeFormat,i,i) <- FixHomoglyph(x)}
    }
    # check number positions
    for(i in c(3,5)){
      x <- substr(strPostcodeFormat,i,i)
      if (str_detect(x,"[^[:digit:]]")){
        substr(strPostcodeFormat,i,i) <- FixHomoglyph(x)}
    }
  }

  # 6 digit postcode format: A**9AA
  if(nchar(strPostcodeFormat) == 6) {
    # check letter positions
    for(i in c(1,5,6)){
      x <- substr(strPostcodeFormat,i,i)
      if (str_detect(x,"[^[:alpha:]]")){
        substr(strPostcodeFormat,i,i) <- FixHomoglyph(x)}
    }
    # check number positions
    for(i in c(4)){
      x <- substr(strPostcodeFormat,i,i)
      if (str_detect(x,"[^[:digit:]]")){
        substr(strPostcodeFormat,i,i) <- FixHomoglyph(x)}
    }
  }

  # 5 digit postcode format: A99AA
  if(nchar(strPostcodeFormat) == 5) {
    # check letter positions
    for(i in c(1,4,5)){
      x <- substr(strPostcodeFormat,i,i)
      if (str_detect(x,"[^[:alpha:]]")){
        substr(strPostcodeFormat,i,i) <- FixHomoglyph(x)}
    }
    # check number positions
    for(i in c(2,3)){
      x <- substr(strPostcodeFormat,i,i)
      if (str_detect(x,"[^[:digit:]]")){
        substr(strPostcodeFormat,i,i) <- FixHomoglyph(x)}
    }
  }


  # return formatted string
  return(strPostcodeFormat)
}
