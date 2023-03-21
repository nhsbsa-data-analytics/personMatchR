#' Create permutations of personal data for matching
#'
#' This is a support function called during the execution of the calc_match_patients function.
#' \cr\cr Calculates permutations of personal information to support joining datasets, allowing
#' data to be initially joined only where the permutations of personal information align.
#' \cr\cr The permutations range from combinations of full fields to only partial information
#' extracted from each piece of personal information.
#'
#' @param df df to be formatted
#' @param forename forename column name
#' @param surname surname column name
#' @param postcode postcode column name
#' @param dob DOB column name
#'
#' @return df with join permutations added
#'
#' @export
#'
calc_permutations <- function(df, forename, surname, postcode, dob) {
  df %>%
    dplyr::mutate(
      # Perm 1-3 require full match of dob with 2 of forename, surname, postcode
      PERM1 = paste0({{ forename }}, {{ surname }}, {{ dob }}),
      PERM2 = paste0({{ forename }}, {{ postcode }}, {{ dob }}),
      PERM3 = paste0({{ surname }}, {{ postcode }}, {{ dob }}),
      # First char forename - 4 chars of surname & postcode
      PERM4 = paste0(
        substr({{ forename }}, 1, 1),
        substr({{ surname }}, 1, 4),
        substr({{ postcode }}, 1, 4)
      ),
      # First 3 chars forename - 2 chars surname & postcode
      PERM5 = paste0(
        substr({{ forename }}, 1, 3),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # Last 3 chars forename - 3 chars of surname & postcode
      PERM6 = paste0(
        substr({{ forename }}, nchar({{ forename }}) - 2, nchar({{ forename }})),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      ),
      # First 3 consonants - 3 chars of surname & postcode
      PERM7 = paste0(
        substr(gsub("[AEIOU]", "", {{ forename }}), 1, 3),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      ),
      # All consonants - 2 chars of surname & postcode
      PERM8 = paste0(
        gsub("[AEIOU]", "", {{ forename }}),
        substr({{ surname }}, 1, 2),
        substr({{ postcode }}, 1, 2)
      ),
      # All vowels - 3 chars of surname & postcode
      PERM9 = paste0(
        gsub("[B-DF-HJ-NP-TV-Z]", "", {{ forename }}),
        substr({{ surname }}, 1, 3),
        substr({{ postcode }}, 1, 3)
      )
    )
}
