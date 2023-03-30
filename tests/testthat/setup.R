#' Helper file for unit testing
#'
#' Allows variables to be defined to be referenced by test cases
#'
#'


# Initialise test mode ------------------------------------------------------------------------
original_test_mode <- getOption("my_package.test_mode")
options("my_package.test_mode" = TRUE)


# Create Function: save_db_table --------------------------------------------------------------
# this funciton will create a copy of the data tables in the specified NHSBSA database
# connections to databases are handled by the con_nhsbsa function in the nhsbsaR package
# tables will be created for each of the datasets used in the testthat process
# as the data formatting functions for db data are slow pre format any data being saved unless
# the data is specifically for testing the db formatting functions
save_db_table <- function(file_name, bsa_database) {

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = bsa_database)

  # load dataset
  load(paste0("./testdata/", file_name))

  # Read to file
  file <- get(gsub(".rda", "", file_name))

  # Save name
  db_table_name <- paste0("PERSONMATCHR_", toupper(gsub(".rda", "", file_name)))

  # apply formatting to data before passing to database
  if (db_table_name %in% c(
    "PERSONMATCHR_MATCH_TEST_INPUT_A_SINGLE",
    "PERSONMATCHR_MATCH_TEST_INPUT_B_INC_EXACT_MATCH",
    "PERSONMATCHR_MATCH_TEST_INPUT_B_EXCL_EXACT_MATCH",
    "PERSONMATCHR_MATCH_TEST_INPUT_A_MULTIPLE",
    "PERSONMATCHR_MATCH_TEST_INPUT_A_NO_MATCH"
  )) {
    file <- file %>%
      format_postcode(., ID, POSTCODE) %>%
      format_name(., FORENAME) %>%
      format_name(., SURNAME) %>%
      format_date(., DOB)
  }

  # See if table exists
  if (DBI::dbExistsTable(con, db_table_name) == TRUE) {
    DBI::dbRemoveTable(con, db_table_name)
  }

  # Create Table
  DBI::dbWriteTable(con, db_table_name, file, temporary = FALSE)

  # Send update to console
  print(paste0("Table created: ", db_table_name))

  # Disconnect
  DBI::dbDisconnect(con)
}


# Create Function: skip_db_tests --------------------------------------------------------------
# this function will be used to check whether the db tests should be skipped or included
skip_db_tests <- function() {
  if (skip_db == TRUE) {
    skip("Database function test skipped")
  }
}

# Confirm database function test parameters ---------------------------------------------------

# check if db functions should be tested
test_db <- NA
while (is.na(test_db) || !(test_db %in% c("Y", "N"))) {
  test_db <- toupper(readline("Check database functions (Y/N)?: "))
}

if (test_db == "Y") {
  # if databases are to be tested ask for connection information (database and cypher)
  confirm_db <- NA
  while (is.na(confirm_db) || confirm_db == "N" || db_connection == "") {
    db_connection <- toupper(readline("Enter database name (e.g. DALP/DWCP): "))
    # check input reflects known database
    if (!(db_connection %in% c("DALP", "DWCP", ""))) {
      confirm_db <- toupper(readline("Database not a known NHSBSA database.  Continue (Y/N): "))
    } else {
      confirm_db <- "Y"
    }
  }
  db_cypher <- NA
  while (is.na(db_cypher) || db_cypher == "") {
    db_cypher <- toupper(readline("Enter database schema for test data storage: "))
  }

  # check if the datasets need to be created in the database environment
  confirm_db_rebuild_data <- NA
  while (is.na(confirm_db_rebuild_data) || !(confirm_db_rebuild_data %in% c("Y", "N"))) {
    confirm_db_rebuild_data <- toupper(readline("Rebuild test data in database environment (Y/N): "))
  }

  # based on response create test cases in database
  if (confirm_db_rebuild_data == "Y") {

    # Get file names
    files_to_save <- list.files(path = "./testdata/")[grepl(".rda", list.files("./testdata/"))]

    # Save all Files
    lapply(X = files_to_save, FUN = save_db_table, bsa_database = db_connection)
  }

  # update flag to skip database tests
  skip_db <- FALSE
} else {
  # update flag to skip database tests
  skip_db <- TRUE
}
