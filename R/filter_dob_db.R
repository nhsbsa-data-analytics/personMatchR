#' Applies all combinations of dob_substr() filters
#'
#' Through doing so, it mirrors an edit distance of 2 on an 8-digit string
#' For this very particular use case, is faster than LV within SQL
#'
#' @param df A df with dates to be formatted
#'
#' @return A df with relevant dates from both columns
#'
#' @export
#'
#' @examples
#' dob_substr(df, dob_one, dob_two, dob_diff
filter_dob_db <- function(df, dob_one, dob_two, dob_score_threshold){

  df %>%
    dplyr::mutate(
      # Generate 8 character-level matching binary scores
      CHAR1 = ifelse(substr({{ dob_one }},1,1) == substr({{ dob_two }},1,1), 1, 0),
      CHAR2 = ifelse(substr({{ dob_one }},2,2) == substr({{ dob_two }},2,2), 1, 0),
      CHAR3 = ifelse(substr({{ dob_one }},3,3) == substr({{ dob_two }},3,3), 1, 0),
      CHAR4 = ifelse(substr({{ dob_one }},4,4) == substr({{ dob_two }},4,4), 1, 0),
      CHAR5 = ifelse(substr({{ dob_one }},5,5) == substr({{ dob_two }},5,5), 1, 0),
      CHAR6 = ifelse(substr({{ dob_one }},6,6) == substr({{ dob_two }},6,6), 1, 0),
      CHAR7 = ifelse(substr({{ dob_one }},7,7) == substr({{ dob_two }},7,7), 1, 0),
      CHAR8 = ifelse(substr({{ dob_one }},8,8) == substr({{ dob_two }},8,8), 1, 0),
      # Total scores and present as DOB difference
      DOB_SCORE =  round((CHAR1 + CHAR2 + CHAR3 + CHAR4 + CHAR5 + CHAR6 + CHAR7 + CHAR8) / 8, 2),
      DOB_SCORE = ifelse(
        # Not exact
        test = {{ dob_one }} != {{ dob_two }} &
          # Yet same year
          substr({{ dob_one }},1,4) == substr({{ dob_two }},1,4) &
          # And month-day values swapped around
          substr({{ dob_one }},5,6) == substr({{ dob_two }},7,8) &
          substr({{ dob_one }},7,8) == substr({{ dob_two }},5,6) &
          # Eliminating 'impossible' dates with a month greater than 12
          as.integer(substr({{ dob_one }},7,8)) <= 12 &
          as.integer(substr({{ dob_two }},7,8)) <= 12,
        # Then atrribute minimum score to still enable confident match
        yes = 0.75,
        no = DOB_SCORE
      )
    ) %>%
    dplyr::filter(DOB_SCORE >= dob_score_threshold) %>%
    dplyr::select(-c(CHAR1, CHAR2, CHAR3, CHAR4, CHAR5, CHAR6, CHAR7, CHAR8))
}
