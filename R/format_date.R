#' Format the date of birth prior to matching
#'
#' Formatting converts date field to string using format YYYYMMDD
#' \cr\cr Can handle dates supplied in a range of formats
#'
#'
#' @param df a dataframe to feed into function
#' @param dob A vector of type character, integer or numeric with date expressions
#'
#' @return A dataframe with cleansed date (converted to YYYYMMDD) information
#' @export
#'
format_date <- function(df, date) {
  df %>%
    dplyr::mutate({{ date }} := ifelse(
      test = is.na({{ date }}) | is.null({{ date }}) | {{ date }} == "",
      yes = NA,
      no = format(lubridate::fast_strptime(
        x = {{ date }},
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
    ))
}
