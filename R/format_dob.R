
#' Convert date to character yyyymmdd format.
#'
#' @param dob A vector of type character, integer or numeric with date expressions
#'
#' @return A formatted date (yyyymmdd)
#' @export
#'
#' @examples
#' format_dob(dob = "2019-Mar-21")
format_dob <- function(dob) {

  if(is.na(dob) || is.null(dob)){
    # handle missing values first
    return (NA)

  } else {

    # Try to parse using lubridate (returns NA quietly if unable to parse)
    dob_final <- lubridate::fast_strptime(
      x = dob,
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
    )

  }

  if(is.na(dob_final)){
    stop("Not a valid date of birth", call. = FALSE)
    #return(NA)
  }

  return(format(dob_final, format = "%Y%m%d"))
}
