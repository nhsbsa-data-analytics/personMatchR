
#' Convert date to character yyyymmdd format.
#'
#' @param dob A vector of type character, integer or numeric with date expressions
#'
#' @return A formatted date (yyyymmdd)
#' @export
#'
#' @examples
#' format_dob(dob = "2019-Mar-21")
suppressWarnings({
  format_dob <- function(dob) {
    # use anytime library
    # handle missing vlues
    if(is.na(dob) || is.null(dob)){
      return (NA)
    }

    if (!is.na(dob) || !is.null(dob)) {
      dob_final <- anytime::anydate(dob)
      # return(format(dob_final, format = "%Y%m%d"))
    }

    if ((!is.na(dob) || !is.null(dob))
               & is.na(anytime::anydate(dob))
               & !is.na(lubridate::dmy(dob))){
      dob_final <- lubridate::dmy(dob)
    }

    if ((!is.na(dob) || !is.null(dob))
        & is.na(anytime::anydate(dob))
        & !is.na(lubridate::ydm(dob))){
      dob_final <- lubridate::ydm(dob)
    }

    if(is.na(dob_final)){
      stop("Not a valid date of birth", call. = FALSE)
    }

    return(format(dob_final, format = "%Y%m%d"))
  }
})
