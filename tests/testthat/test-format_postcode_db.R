


# Set up connection to the DB
con <- nhsbsaR::con_nhsbsa(database = "DALP")

test_run <- readRDS("tests/testthat/testdata/test_postcode_input.rds")




TEST_RUN <- dbplyr::src_memdb() %>% copy_to(test_run, overwrite = TRUE)

TEST_RUN


expect_results <- dbplyr::memdb_frame(readRDS("tests/testthat/testdata/test_postcode_expected.rds"))

test_run

df_one

format_postcode_db(TEST_RUN, POSTCODE)
test_run


setwd("tests/testthat/testdata/")

files_to_save = list.files()[grepl(".rds", list.files())]

file_name = files_to_save[1]

save_db_table <- function(file_name){

  # Set up connection to the DB
  con <- nhsbsaR::con_nhsbsa(database = "DALP")

  # Read File
  file = readRDS(file_name)

  # Field names
  fields = colnames(file)

  # Save name
  db_table_name = toupper(gsub(".rds", "", file_name))

  # See if table exissts
  if(DBI::dbExistsTable(con, db_table_name) == TRUE){
    DBI::dbRemoveTable(con, db_table_name)
  }

  #Create Table
  DBI::dbWriteTable(con, "ZZZ", file)

  #Create Table
  DBI::dbAppendTable(con, db_table_name, file)

  #Disconnect
  DBI::dbDisconnect(con)
}

?DBI::dbCreateTable

toupper(gsub(".rds", "", file))

save_db_table(files_to_save[1])

DBI::dbCreateTable(con, "XYZ_XYZ", file)

DBI::dbCreateTable(con, file, "XYZ_XYZ")



save_db_table("tests/testthat/testdata/test_postcode_input.rds", "TEST_123")

?DBI::dbCreateTable
