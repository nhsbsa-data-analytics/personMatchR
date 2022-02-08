#' Applies a filter on DOB to workaround LV-edit distance runtime
#'
#' Format the name strings prior to matching
#' Df must include cols: DOB_ONE & DOB_TWO
#'
#' @param df A df to be formatted
#' @param sub_one first character index
#' @param sub_two second character index
#' @param sub_three third character index
#'
#' @return A df with filter applied to DOB
#'
#' @export
#'
#' @examples
#' dob_substr(df, sub_one, sub_two, sub_three)
dob_substr = function(df, sub_one, sub_two, sub_three){

  df %>%
    filter(
      SUBSTR(DOB_ONE, sub_one, 1) == SUBSTR(DOB_TWO, sub_one, 1) |
        # Tokens share same second letter
        SUBSTR(DOB_ONE, sub_two, 1) == SUBSTR(DOB_TWO, sub_two, 1) |
        # Tokens share same second letter
        SUBSTR(DOB_ONE, sub_three, 1) == SUBSTR(DOB_TWO, sub_three, 1)
    )
}
