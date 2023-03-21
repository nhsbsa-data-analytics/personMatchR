#' Filter data on name to limit cross-join
#'
#' This is a support function called during the execution of the calc_match_patients function.
#' \cr\cr Applies a filter on name to limit volume of data from cross-join.
#' \cr\cr Limits cross-joins to only include name-pair instances that share certain characteristics.
#' This includes cases where the two names contain the same 1st, 2nd or last letter, or being a
#' sub-string of the other name.
#' \cr\cr This will prevent any records being considered for matching where the names are notably
#' different which would mean no match would be expected.
#'
#' @param df a dataframe to feed into function
#' @param name_one the field containing the first name column
#' @param name_two the field containing the second name column
#'
#' @return A df filtered to only include suitable combinations of names
#'
#' @export
#'
filter_name <- function(df, name_one, name_two) {
  df %>%
    dplyr::filter(
      # Tokens share the same first letter
      substr({{ name_one }}, 1, 1) == substr({{ name_two }}, 1, 1) |
        # Tokens share same second letter
        substr({{ name_one }}, 2, 2) == substr({{ name_two }}, 2, 2) |
        # Tokens share same last letter
        substr({{ name_one }}, nchar({{ name_one }}), nchar({{ name_one }})) == substr({{ name_two }}, nchar({{ name_two }}), nchar({{ name_two }})) |
        # One token is a substring of the other
        stringr::str_detect({{ name_two }}, {{ name_one }}) |
        stringr::str_detect({{ name_one }}, {{ name_two }})
    )
}
