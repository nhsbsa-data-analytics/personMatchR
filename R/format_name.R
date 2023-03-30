#' Format name strings prior to matching
#'
#' Suitable for formatting forename/surname strings prior to matching
#' \cr\cr Formatting includes conversion to upper case and removal of non-alphabetic characters
#'
#' @param df a dataframe to feed into function
#' @param name a string field to be cleansed
#'
#' @return A dataframe with cleansed name string
#'
#' @export
#'
format_name <- function(df, name) {
  df %>%
    dplyr::mutate(
      # Remove non-alpha chars and convert emtpy string to NA
      {{ name }} := toupper(gsub("[^[:alpha:]]", "", {{ name }})),
      {{ name }} := ifelse({{ name }} == "", NA, {{ name }})
    )
}
