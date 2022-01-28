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
  ifelse(grepl("/", dob), gsub("/", "-", dob), dob)

  # If contains letter
  if(!grepl("[a-zA-Z]", dob)){

    dob = as.Date(dob, format = '%Y-%m-%d')

    }else if(grepl("[a-zA-Z]", dob)){

      dob = as.Date(dob, format = '%d-%B-%y')

    }else{

      dob = as.Date('2000-01-01')

    }

  print(dob)

  # Change future dates to past
  ifelse(dob > Sys.Date(), format(dob, "19%y-%m-%d"), format(dob))

  # Convert to character
  dob = format(dob, format = '%Y%m%d')

  return(dob)
}


ifelse(grepl("/", '27/feb/67'), gsub("/", "-", '27/feb/67'), '27/feb/67')


as.Date('27-feb-94', format = '%d-%B-%Y')

format_dob('27/feb/67')
format_dob('1994/02/12')
format_dob('27/feb/1994')


ifelse("2047-02-27" > Sys.Date(), format("2047-02-27", "19%y-%m-%d"), format("2047-02-27"))


z=format_dob('27-feb-47')

test_dataset_b %>%
  mutate(
    DOB = format_dob(DOB)
  )


test_dataset_a %>%
  mutate(TMP = grepl("a-zA-Z", DOB))
