#' Applies a filter on either forename or surname to limit cross-join
#'
#' Only contains name-pair instances that share certain characteristsics
#' These include same 1st, 2nd or last letter, or being a substring of another
#'
#' @param df A df to be formatted
#' @param name_one first name column
#' @param name_two second name column
#'
#' @return A df with a filtered name-col to limit post cross-join
#'
#' @export
#'
#' @examples
#' name_db_filter(df, name_one, name_two)
filter_name_db <- function(df, name_one, name_two){

  df %>%
    dplyr::filter(
      # Tokens share the same first letter
      SUBSTR({{ name_one }}, 1, 1) == SUBSTR({{ name_two }}, 1, 1) |
        # Tokens share same second letter
        SUBSTR({{ name_one }}, 2, 1) == SUBSTR({{ name_two }}, 2, 1) |
        # Tokens share same last letter
        SUBSTR({{ name_one  }}, LENGTH({{ name_one }}), 1) == SUBSTR({{ name_two }}, LENGTH({{ name_two }}), 1) |
        # One token is a substring of the other
        INSTR({{ name_one }}, {{ name_two }}) > 0 |
        INSTR({{ name_two }}, {{ name_one }}) > 0
    )
}
