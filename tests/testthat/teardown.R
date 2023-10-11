#' Helper file for unit testing
#'
#' Allows variables to be defined to be referenced by test cases
#'
#'


# Create Function: remove_db_table --------------------------------------------------------------
# this function will remove copies of the test data tables in the specified NHSBSA database
# connections to databases are handled by the con_nhsbsa function in the nhsbsaR package
# tables will be removed relating to each of the datasets used in the testthat process
remove_db_table <- function(file_name, bsa_database) {

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = bsa_database)

  # Identify the database table to remove
  db_table_name <- paste0("PERSONMATCHR_", toupper(gsub(".rda", "", file_name)))

  # See if table exists and if so remove it
  if (DBI::dbExistsTable(con, db_table_name) == TRUE) {
    DBI::dbRemoveTable(con, db_table_name)
  }

  # Send update to console
  print(paste0("Table removed: ", db_table_name))

  # Disconnect
  DBI::dbDisconnect(con)
}



# Check if database tables should be removed ------------------------------

# only check for data removal if the database tables have been checked
if (test_db == "Y") {

  # check if the datasets need to be created in the database environment
  confirm_db_remove_data <- NA
  while (is.na(confirm_db_remove_data) || !(confirm_db_remove_data %in% c("Y", "N"))) {
    confirm_db_remove_data <- toupper(readline("Remove test data from database environment (Y/N): "))
  }

  # based on response create test cases in database
  if (confirm_db_remove_data == "Y") {

    # Get file names
    files_to_remove <- list.files(path = "./testdata/")[grepl(".rda", list.files("./testdata/"))]

    # Save all Files
    lapply(X = files_to_remove, FUN = remove_db_table, bsa_database = db_connection)
  }

}



# Reset test mode -----------------------------------------------------------------------------
options("my_package.test_mode" = original_test_mode)
