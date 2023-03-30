#' Filter data on name to limit cross-join
#'
#' This is a support function called during the execution of the calc_match_person_db function.
#' \cr\cr Applies a filter on name to limit volume of data from cross-join.
#' \cr\cr Limits cross-joins to only include name-pair instances that share certain characteristics.
#' This includes cases where the two names contain the same 1st, 2nd or last letter, or being a
#' sub-string of the other name.
#' \cr\cr This will prevent any records being considered for matching where the names are notably
#' different which would mean no match would be expected.
#'
#' @param df a 'lazyframe' generated from a database connection
#' @param name_one the field containing the first name column
#' @param name_two the field containing the second name column
#'
#' @return a 'lazyframe' object filtered to only include suitable combinations of names
#'
#' @export
#'
filter_name_db <- function(df, name_one, name_two) {
  df %>%
    dplyr::filter(
      # Tokens share the same first letter
      SUBSTR({{ name_one }}, 1, 1) == SUBSTR({{ name_two }}, 1, 1) |
        # Tokens share same second letter
        SUBSTR({{ name_one }}, 2, 1) == SUBSTR({{ name_two }}, 2, 1) |
        # Tokens share same last letter
        SUBSTR({{ name_one }}, LENGTH({{ name_one }}), 1) == SUBSTR({{ name_two }}, LENGTH({{ name_two }}), 1) |
        # One token is a substring of the other
        INSTR({{ name_one }}, {{ name_two }}) > 0 |
        INSTR({{ name_two }}, {{ name_one }}) > 0
    )
}
