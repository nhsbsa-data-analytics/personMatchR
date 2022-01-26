#' Fix common homoglyph errors that appear
#'
#' @param string A string to be corrected
#' @param char_pos The character index position to be corrected
#' @param character_or_number Whether character or numbers should be corrected
#'
#' @return A character where known homoglyphs have been switched
#' @export
#'
#' @examples
#' fix_homoglyph(string, char_pos, character_or_number)
fix_homoglyph <- function(string, char_pos, character_or_number){

  # Helper function to replace numbers for characters
  replace_numbers = function(x){
    stringr::str_replace_all(x, c("0" = "O", "5" = "S"))
  }

  # Helper function to replace characters for numbers
  replace_characters = function(x){
    stringr::str_replace_all(x, c("O" = "0", "I" = "1", "L" = "1", "S" = "5"))
  }

  # Split string into individual letters, in order they can indexed
  char = strsplit(string, "")[[1]]

  # At selected index position, apply either of the 2 above helper functions
  if(character_or_number == "character"){
    char[char_pos] = replace_characters(char[char_pos])
  }else{
    char[char_pos] = replace_numbers(char[char_pos])
  }

  # Post-correction, now paste letters back into single string
  char <- paste(char, collapse = '')
}
