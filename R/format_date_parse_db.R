#' Format a DOB within the DB: Parsing collected data then pushed back
#'
#' Formatting converts date field to string using format YYYYMMDD
#' \cr\cr Can handle dates supplied in a range of formats
#' \cr\cr This function will not work with sizeable database tables due to having to copy the table
#' in memory
#'
#' @param df a 'lazyframe' generated from a database connection with date field to be formatted
#' @param date a date column to be formatted
#'
#' @return a 'lazyframe' object with cleansed date of birth field
#'
#' @export
#'
format_date_parse_db <- function(df, date) {

  # Format distinct collected dates
  dates <- df %>%
    dplyr::select({{ date }}) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::mutate(
      TMP = as.character({{ date }}),
      TMP = ifelse(
        test = is.na(TMP) | is.null(TMP) | TMP == "",
        yes = NA,
        no = format(lubridate::fast_strptime(
          x = TMP,
          # American way last
          format = c(
            # Ymd
            "%Y-%m-%d",
            "%Y%m%d",
            "%Y/%m/%d",
            "%Y %m %d",
            "%Y-%b-%d",
            "%Y%b%d",
            "%Y/%b/%d",
            "%Y %b %d",
            # dmY
            "%d-%m-%Y",
            "%d%m%Y",
            "%d/%m/%Y",
            "%d %m %Y",
            "%d-%b-%Y",
            "%d%b%Y",
            "%d/%b/%Y",
            "%d %b %Y",
            # dmy
            "%d-%m-%y",
            "%d%m%y",
            "%d/%m/%y",
            "%d %m %y",
            "%d-%b-%y",
            "%d%b%y",
            "%d/%b/%y",
            "%d %b %y",
            # ymd
            "%y-%m-%d",
            "%y%m%d",
            "%y/%m/%d",
            "%y %m %d",
            "%y-%b-%d",
            "%y%b%d",
            "%y/%b/%d",
            "%y %b %d",
            # Ydm
            "%Y-%d-%m",
            "%Y%d%m",
            "%Y/%d/%m",
            "%Y %d %m",
            "%Y-%d-%b",
            "%Y%d%b",
            "%Y/%d/%b",
            "%Y %d %b",
            # mdY
            "%m-%d-%Y",
            "%m%d%Y",
            "%m/%d/%Y",
            "%m %d %Y",
            "%b-%d-%Y",
            "%b%d%Y",
            "%b/%d/%Y",
            "%b %d %Y",

            # mdy
            "%m-%d-%y",
            "%m%d%y",
            "%m/%d/%y",
            "%m %d %y",
            "%b-%d-%y",
            "%b%d%y",
            "%b/%d/%y",
            "%b %d %y"
          )
        ),
        format = "%Y%m%d"
        )
      )
    ) %>%
    dbplyr::memdb_frame()

  # Rejoin and rename dates
  df <- df %>%
    dplyr::left_join(dates, copy = TRUE) %>%
    dplyr::select(-{{ date }}) %>%
    dplyr::rename({{ date }} := TMP)
}
