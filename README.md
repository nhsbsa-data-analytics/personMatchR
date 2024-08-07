
# personMatchR

Helper package for matching individuals across two datasets. This R
package has been developed by the NHS Business Services Authority Data
Science team.

The package is a series of functions that process and then match person
information fields across two datasets, either in data frames or
database tables.

The matching functions will identify whether a record containing person
information can be matched to any records in a second dataset, either
based on identical person information or a close match based on similar
information.

The matching process focuses on comparing individuals based on four key
pieces of information:

- Forename
- Surname
- Date of Birth
- Postcode

The quality of matching accuracy will be heavily influenced by the
formatting of the input data, and there are processing functions
available within the package to support this ([see data
preparation](#data-preparation)).

The personMatchR package has different functions available to handle
matching whether the input data is held within data frames or via a
connection to database tables:

- calc_match_person
  - suitable for data-frames containing up to one million records
- calc_match_person_db
  - currently only set up and tested for Oracle database infrastructure
    used by NHSBSA
  - suitable for large volume datasets
  - data formatting will need to be handled prior to matching, with
    functions available in the package to support this
- calc_match_person_self (added in version 1.1.0.0)
  - suitable for data-frames containing up to one million records
  - allows a single data-frame to be matched against itself
  - will include both exact and confident matches for each record
    (excluding same ID)
- calc_match_person_self_db (added in version 1.1.0.0)
  - currently only set up and tested for Oracle database infrastructure
    used by NHSBSA
  - suitable for large volume datasets
  - data formatting will need to be handled prior to matching, with
    functions available in the package to support this
  - allows a single data-frame to be matched against itself
  - will include both exact and confident matches for each record
    (excluding same ID)

## Installation

You can install the development version of personMatchR from the NHSBSA
Data Analytics [GitHub](https://github.com/nhsbsa-data-analytics/) with:

``` r
# install.packages("devtools")
devtools::install_github("nhsbsa-data-analytics/personMatchR")
```

## Documentation

In addition to function help files, some additional documentation has
been included to provide detailed information about the package
functions and some examples of the package in use:

- [Package Usage Blog](documentation/personMatchR%20Usage%20Blog.pdf)
  - Basic overview of package, including example test case

## Requirements

The datasets being matched each require a forename, surname, DOB and
postcode field to be present. In addition, each dataset also requires a
unique identification field to be present. Users will have to generate
such a field prior to using the matching function if one is not already
present.

## Example

The following basic example shows how to match between two data-frames
that are available within the [documentation folder](documentation/) for
this package.

``` r
df_A <- personMatchR::TEST_DF_A
head(df_A)
```

    ##   ID SURNAME FORENAME POSTCODE        DOB
    ## 1  1 O'Brien  Richard AA9A 9AA 1942-03-25
    ## 2  2  Bloggs      Joe  A9A 9AA 1999-01-01
    ## 3  3  Watson    David   A9 9AA 2001-11-01
    ## 4  4 Neville    Dylan  A99 9AA 1941-05-24

``` r
df_B <- personMatchR::TEST_DF_B
head(df_B)
```

    ##   ID SURNAME FORENAME POSTCODE        DOB
    ## 1  1 O Brien  Richard AA9A 9AA 1942-03-25
    ## 2  2  Bloggs   Joseph  A9A 9AA 1999-01-01
    ## 3  3   Brown     John   Z1 1ZZ 2001-11-01
    ## 4  4   Dylan  Neville  A99 9AA 1941-05-24

With such a small set of data it is easy to manually compare these two
datasets, where it is clear that records are similar between both
datasets:

- Record 1 in both datasets only differs by an apostrophe in the surname
  field
- Record 2 in both datasets only has a different version of the forename
- Record 3 in dataset A does not appear anywhere in dataset B
- Record 4 in both datasets only differs by the forename and surname
  being swapped

When matching these datasets we would hope that records 1, 2 and 4 are
matched.

We can pass the datasets to the calc_match_person function and review
the output. For this example we will set parameters to only return the
key fields from the matching, format the data prior to matching and
include records without a match in the output:

``` r
library(personMatchR)
library(dplyr)
df_output <- personMatchR::calc_match_person(
  df_one = df_A, # first dataset
  id_one = ID, # unique id field from first dataset
  forename_one = FORENAME, # forename field from first dataset
  surname_one = SURNAME, # surname field from first dataset
  dob_one = DOB, # date of birth field from first dataset
  postcode_one = POSTCODE, # postcode field from first dataset
  df_two = df_B, # second dataset
  id_two = ID, # unique id field from second dataset
  forename_two = FORENAME, # forename field from second dataset
  surname_two = SURNAME, # surname field from second dataset
  dob_two = DOB, # date of birth field from second dataset
  postcode_two = POSTCODE, # postcode field from second dataset
  output_type = "key", # only return the key match results
  format_data = TRUE, # format input datasets prior to matching
  inc_no_match = TRUE # return records from first dataset without matches
)
```

    ## # A tibble: 4 × 5
    ##   DF1_INPUT_ID DF2_INPUT_ID MATCH_TYPE MATCH_COUNT MATCH_SCORE
    ##   <chr>        <chr>        <chr>            <dbl>       <dbl>
    ## 1 2            2            Confident            1        0.96
    ## 2 1            1            Exact                1        1   
    ## 3 4            4            Exact                1        1   
    ## 4 3            <NA>         No Match             0        0

The match results show exact matches for records 1 and 4 as the only
differences were special characters and transposition of names. For
record 2 the results show a confident match, as although not identical
the names were similar enough to pass the confidence thresholds. As
expected, record 3 does not produce any matches.

### Understanding match output: MATCH_COUNT & MATCH_SCORE

These fields in the output provide context for the match results:

- MATCH_COUNT
  - Shows the number of matches found for each record (each match will
    be included in the output).
  - Where this is greater than one, some additional handling may be
    required to review.
- MATCH_SCORE
  - Provides a general weighted score for the match which may help
    compare instances where multiple potential matches are identified.
  - Please note that this is for guide purposes only, and a higher score
    will not always mean that the match is more likely to be correct.
    Weightings for each part of the matching can be adjusted using
    parameters in the function call.

## Data preparation

In the example above the input data was passed through some formatting
functions as part of the main matching package function call
(format_data = TRUE). This option is only available when matching across
data frames using the calc_match_person() function. However, the
formatting functions could be called individually prior to calling the
matching function.

There are three matching functions available, with different versions
available for the database matching function:

- format_name() / format_name_db()
- format_date() / format_date_db()
- format_postcode() / format_postcode_db()

The following code shows how these functions could be used to format the
data prior to matching:

``` r
df_A = df_A %>% 
  format_date(date = DOB) %>% 
  format_name(name = FORENAME) %>% 
  format_name(name = SURNAME) %>% 
  format_postcode(id = ID, postcode = POSTCODE)
head(df_A)
```

    ## # A tibble: 4 × 5
    ##   ID    SURNAME FORENAME DOB      POSTCODE
    ##   <chr> <chr>   <chr>    <chr>    <chr>   
    ## 1 1     OBRIEN  RICHARD  19420325 AA9A9AA 
    ## 2 2     BLOGGS  JOE      19990101 A9A9AA  
    ## 3 3     WATSON  DAVID    20011101 A99AA   
    ## 4 4     NEVILLE DYLAN    19410524 A999AA

The calc_match_person_db() function **does not** offer the option to
format the data prior to matching, with users required to carry out the
processing as above beforehand.

It is **strongly** encouraged that users create new database tables
after processing these fields. They can then use these freshly created
tables as the matching function input, which will typically
significantly improve runtime performance.

## Match function parameter: output_type

This parameter will determine the number of fields from each dataset
returned in the output:

- key
  - this option will only return the ID fields from each dataset, plus
    the match outcome and match score
  - this output could then be used to join to original datasets as
    required
- match
  - this option will return the “key” fields plus the personal
    information fields used in the matching
  - this output would allow the matches to easily be reviewed
- all
  - will return the key fields, plus all other fields from both datasets
    where a match was found

## Match function parameter: format_data (not used for database match functions)

This parameter will determine whether or not the data is formatted as it
is passed to the matching function. Formatting the data can help ensure
both datasets are consistently formatted, accounting for things like
case, removal of special characters, date of birth and postcode
patterns. As formatted data is likely to have better matching outcomes
it is strongly advised to apply this option.

- TRUE
  - (RECOMMENDED) apply formatting to input data
- FALSE
  - do not apply formatting to input data
  - this is likely to limit match results
  - formatting functions (format_name, format_dob, format_dob) could be
    applied individually outside of the matching function, prior to
    passing data to main matching function

## Match function parameter: inc_no_match

This parameter will determine whether or not the output results will
include details of records where no match could be found:

- TRUE
  - all records from the initial dataset will be included in the output,
    even if no match was found
- FALSE
  - the output will only include records where a match could be
    identified

## Match function parameter: unique_combinations_only (self join only)

This parameter will determine whether or not the output results will
include unique combinations or both versions of a potential match (e.g
A=B & B=A).

The MATCH_COUNT value will represent the number of matches remaining
after the data has been limited to unique combinations of potential
matches.

- TRUE
  - Results will be limited so that each match pair will only be
    included once in the output
  - This may be useful to prevent the same potential match appearing
    twice
- FALSE
  - Both versions of each potential match (e.g. A=B and B=A) will be
    included in the output
