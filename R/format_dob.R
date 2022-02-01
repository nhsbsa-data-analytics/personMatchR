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

  # handle missing variables
  if(is.na(dob)||is.null(dob)){
    return(NA)
  }

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
    dob = NA
  }

  # Change date to character is it isnt NA
  if(is.na(dob) == F){dob = format(dob, format = '%Y%m%d')}
}
