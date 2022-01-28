#' Format a DOB
#'
#' Format the date of birth prior to matching
#' Convert to a string representation in the format YYYYMMDD
#'
#' @param dob A date of birth date field to be cleansed
#'
#' @return A cleansed string
#'
#' @export
#'
#' @examples
#' format_dob(dob)
format_dob <- function(dob){

  # Return if DOB is NA
  ifelse(is.na(dob), return(dob), dob)

  # Swap slash for hyphen
  if(grepl('/', dob)){dob = gsub('/', '-', dob)}

  # If contains letter
  if(!grepl("[a-zA-Z]", dob)){

    # Then use date with month-as-character format
    dob = as.Date(dob, format = '%Y-%m-%d')

  }else if(grepl("[a-zA-Z]", dob)){

    # Or, use date-as-number format
    dob = as.Date(dob, format = '%d-%B-%y')

  }else{

    # Attribute arbitrary day for other records
    dob = as.Date('1900-01-01')

  }

  # Check if date in future dates
  if(dob > Sys.Date()){

    # If so, change century then format as character
    dob = format(dob, "19%y%m%d")

  }else{

    # If not formmat as character
    dob = format(dob, format = '%Y%m%d')

  }
}
