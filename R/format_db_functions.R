#' Formatting either a Forename or Surname within the DB
#'
#' Format the name strings prior to matching
#' Formatting includes conversion to upper case and removal of
#' non alphabetic characters
#'
#' @param df A df to be formatted
#' @param name_col a patient name column
#'
#' @return A df with cleansed patient name information
#'
#' @export
#'
#' @examples
#' format_db_name(df, name_col)
format_db_name <- function(df, name_col){

  df %>%
    mutate(
      # Remove non-alpha chars and convert emtpy string to NA
      {{ name_col }} := toupper(REGEXP_REPLACE({{ name_col }}, "[^[:alpha:]]", "")),
      {{ name_col }} := ifelse(nchar({{ name_col }}) == 0, NA, {{ name_col }})
    )
}

#' Format a DOB within the DB
#'
#' Format the date of birth prior to matching
#' Convert to a string representation in the format YYYYMMDD
#'
#' @param df A df to be formatted
#' @param name_col a date column
#'
#' @return A df with cleansed date information, as a 8-digit number
#'
#' @export
#'
#' @examples
#' format_db_date(df, date_col)
format_db_date <- function(df, date_col){

  df %>%
    mutate(
      {{ date_col }} := case_when(
        REGEXP_INSTR({{ date_col }}, '[A-Z]') == 0 ~ as.numeric({{ date_col }}),
        REGEXP_INSTR({{ date_col }}, '[A-Z]') == 1 ~ as.numeric(TO_CHAR({{ date_col }}, "YYYYMMDD")),
        nchar({{ date_col }}) == 0 ~ NA
        )
      )
}

#' Format the postcode strings prior to matching
#'
#' Formatting includes conversion to upper case and removal of
#' non alphanumeric characters
#'
#' @param df A df with postcode to be formatted
#' @param postcode_col the postcode column to be formatted
#'
#' @return A cleansed string
#' @export
#'
#' @examples
#' format_db_postcode(df, postcode_col)
format_db_postcode <- function(df, postcode_col){

  # Postcode formatting & Generate Arbitrary ID
  df <- df %>%
    rename({{ postcode_col }} := POSTCODE) %>%
    window_order(POSTCODE) %>%
    group_by(POSTCODE) %>%
    mutate(ID = dense_rank(POSTCODE)) %>%
    ungroup()

  # Just process distinct postcodes
  output <- df %>%
    select(ID, POSTCODE) %>%
    distinct()

  # Postcode formatting
  output <- output %>%
    select(ID, POSTCODE) %>%
    distinct() %>%
    filter(!is.na(POSTCODE)) %>%
    mutate(
      # Format and split postcode
      POSTCODE = ifelse(nchar(POSTCODE) == 0, NA, POSTCODE),
      POSTCODE= toupper(REGEXP_REPLACE(POSTCODE, "[^[:alnum:]]", "")),
      POSTCODE = trimws(REGEXP_REPLACE(POSTCODE, '*', ' '))
    ) %>%
    nhsbsaR::oracle_unnest_tokens(col = 'POSTCODE') %>%
    filter(!is.na(TOKEN)) %>%
    group_by(ID) %>%
    mutate(LEN = n()) %>%
    ungroup() %>%
    mutate(
      TOKEN = case_when(
        # Postcode Length 7
        LEN == 7 & TOKEN_NUMBER == 1 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 7 & TOKEN_NUMBER == 2 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 7 & TOKEN_NUMBER == 3 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 7 & TOKEN_NUMBER == 5 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 7 & TOKEN_NUMBER == 6 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 7 & TOKEN_NUMBER == 7 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        # Postcode Length 6
        LEN == 6 & TOKEN_NUMBER == 1 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 6 & TOKEN_NUMBER == 4 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 6 & TOKEN_NUMBER == 5 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 6 & TOKEN_NUMBER == 6 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        # Postode Length 5
        LEN == 5 & TOKEN_NUMBER == 1 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 5 & TOKEN_NUMBER == 2 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 5 & TOKEN_NUMBER == 3 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 5 & TOKEN_NUMBER == 4 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 5 & TOKEN_NUMBER == 5 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        # Postcode Remaining
        T ~ TOKEN
      )
    ) %>%
    select(-LEN)

  # Pull the DB connection
  db_connection <- output$src$con

  # Build SQL Query
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "
    SELECT ID,
    LISTAGG(TOKEN, '') within group (order by TOKEN_NUMBER) as POSTCODE
    FROM (", dbplyr::sql_render(output), ")
    GROUP BY ID
    "
  )

  # Query Output
  output <- dplyr::tbl(src = db_connection, dplyr::sql(sql_query))

  # Rejoin back to original df then return
  df <- df %>%
    select(-POSTCODE) %>%
    left_join(output, by = "ID") %>%
    select(-ID) %>%
    rename(POSTCODE := {{ postcode_col }} )
  return(df)
}

#' Temporary convenient function that formats PDS with minimum mutates()
#'
#' Formatting includes surname, forename, dob and postcode
#' Df must contain: DOB, FORENAME, SURNAME, POSTCODE, NHS_NO_PDS
#'
#' @param df A df with postcode to be formatted
#' @param postcode_col the postcode column to be formatted
#'
#' @return A cleansed PDS df, ready for matching functions
#' @export
#'
#' @examples
#' format_db_pds_temp(df)
format_db_pds_temp <- function(df){

  df <- df %>%
    select(DOB, FORENAME, SURNAME, POSTCODE, ID = NHS_NO_PDS) %>%
    mutate(
      # Remove non-alpha chars and convert emtpy string to NA
      FORENAME = toupper(REGEXP_REPLACE(FORENAME, "[^[:alpha:]]", "")),
      FORENAME = ifelse(nchar(FORENAME) == 0, NA, FORENAME),
      SURNAME = toupper(REGEXP_REPLACE(SURNAME, "[^[:alpha:]]", "")),
      SURNAME = ifelse(nchar(SURNAME) == 0, NA, SURNAME),
      # PDS DOB Format
      DOB = as.numeric(TO_CHAR(DOB, "YYYYMMDD")),
      DOB = ifelse(nchar(DOB) == 0, NA, DOB),
      # Format and split postcode
      POSTCODE = ifelse(nchar(POSTCODE) == 0, NA, POSTCODE),
      POSTCODE = toupper(REGEXP_REPLACE(POSTCODE, "[^[:alnum:]]", "")),
      POSTCODE = trimws(REGEXP_REPLACE(POSTCODE, '*', ' '))
    ) %>%
    nhsbsaR::oracle_unnest_tokens(col = 'POSTCODE') %>%
    filter(!is.na(TOKEN)) %>%
    group_by(ID) %>%
    mutate(LEN = max(TOKEN_NUMBER)) %>%
    ungroup() %>%
    mutate(
      TOKEN = case_when(
        # Postcode Length 7
        LEN == 7 & TOKEN_NUMBER == 1 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 7 & TOKEN_NUMBER == 2 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 7 & TOKEN_NUMBER == 3 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 7 & TOKEN_NUMBER == 5 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 7 & TOKEN_NUMBER == 6 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 7 & TOKEN_NUMBER == 7 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        # Postcode Length 6
        LEN == 6 & TOKEN_NUMBER == 1 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 6 & TOKEN_NUMBER == 4 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 6 & TOKEN_NUMBER == 5 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 6 & TOKEN_NUMBER == 6 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        # Postode Length 5
        LEN == 5 & TOKEN_NUMBER == 1 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 5 & TOKEN_NUMBER == 2 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 5 & TOKEN_NUMBER == 3 ~ REPLACE(REPLACE(REPLACE(REPLACE(TOKEN, 'O', '0'), 'I', '1'), 'L', '1'), 'S', '5'),
        LEN == 5 & TOKEN_NUMBER == 4 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        LEN == 5 & TOKEN_NUMBER == 5 ~ REPLACE(REPLACE(TOKEN, '5', 'S'), '0', 'O'),
        # Postcode Remaining
        T ~ TOKEN
      )
    ) %>%
    select(-LEN)

  # Pull the DB connection
  db_connection <- df$src$con

  # Build SQL Query
  sql_query <- dbplyr::build_sql(
    con = db_connection,
    "SELECT
    ID, DOB, FORENAME, SURNAME,
    LISTAGG(TOKEN, '') within group (order by TOKEN_NUMBER) as POSTCODE
    FROM (", dbplyr::sql_render(df), ")
    GROUP BY
    ID, DOB, FORENAME, SURNAME"
  )

  # Return
  df <- dplyr::tbl(src = db_connection, dplyr::sql(sql_query))
  return(df)
}
