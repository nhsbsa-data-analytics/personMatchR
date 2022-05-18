# Save all test datasets as Database tables

# Set directory
setwd("tests/testthat/testdata/")

# Get file names
files_to_save = list.files()[grepl(".rds", list.files())]

# Function
save_db_table <- function(file_name){

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")

  # Read File
  file = as.data.frame(readRDS(file_name))

  # Save name
  db_table_name = toupper(gsub(".rds", "", file_name))

  # See if table exists
  if(DBI::dbExistsTable(con, db_table_name) == TRUE){
    DBI::dbRemoveTable(con, db_table_name)
  }

  #Create Table
  DBI::dbWriteTable(con, db_table_name, file, temporary = FALSE)

  #Disconnect
  DBI::dbDisconnect(con)
}

# Save all Files
lapply(X = files_to_save, FUN = save_db_table)
