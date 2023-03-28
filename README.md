
# personMatchR

Helper package for matching individuals across two data-sets. This R
package has been developed by the NHS Business Services Authority Data
Science team.

The matching functions will identify whether a record containing person
information can be matched to any records in a second data-set, either
based on identical person information or a close match based on similar
information.

The matching process focuses on comparing individuals based on four key
pieces of information:

- Forename
- Surname
- Date of Birth
- Postcode

The personMatchR package has different functions available to handle
matching, whether the input data is held within data frames or via a
connection to database tables:

- calc_match_person
  - suitable for data-frames containing up to one million records
- calc_match_person_db
  - currently only set up and tested for Oracle database infrastructure
    used by NHSBSA
  - suitable for large volume data-sets
  - data formatting will need to be handled prior to matching, with
    functions available in the package to support this

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

## Example

The following basic example shows how to match between two data-frames
that are available within the [documentation folder](documentation/) for
this package.

``` r
df_A <- readRDS("documentation/TEST_DF_A.rds")
head(df_A)
```

    ##   ID SURNAME FORENAME POSTCODE        DOB
    ## 1  1 O'Brien  Richard AA9A 9AA 1942-03-25
    ## 2  2  Bloggs      Joe  A9A 9AA 1999-01-01
    ## 3  3  Watson    David   A9 9AA 2001-11-01
    ## 4  4 Neville    Dylan  A99 9AA 1941-05-24

``` r
df_B <- readRDS("documentation/TEST_DF_B.rds")
head(df_B)
```

    ##   ID SURNAME FORENAME POSTCODE        DOB
    ## 1  1 O Brien  Richard AA9A 9AA 1942-03-25
    ## 2  2  Bloggs   Joseph  A9A 9AA 1999-01-01
    ## 3  3   Brown     John   Z1 1ZZ 2001-11-01
    ## 4  4   Dylan  Neville  A99 9AA 1941-05-24

With such a small set of data it is easy to manually compare these two
data-sets, where it is clear that records are similar between both
data-sets:

- Record 1 in both data-sets only differs by an apostrophe in the
  surname field
- Record 2 in both data-sets only has a different version of the
  forename
- Record 3 in data-set A does not appear anywhere in data-set B
- Record 4 in both data-sets only differs by the forename and surname
  being swapped

When matching these data-sets we would hope that records 1, 2 and 4 are
matched.

We can pass the data-sets to the calc_match_person function and review
the output. For this example we will set parameters to only return the
key fields from the matching, format the data prior to matching and
include records without a match in the output:

``` r
library(personMatchR)
library(dplyr)
df_output <- personMatchR::calc_match_person(
  df_one = df_A, # first data-set
  id_one = ID, # unique id field from first data-set
  forename_one = FORENAME, # forename field from first data-set
  surname_one = SURNAME, # surname field from first data-set
  dob_one = DOB, # date of birth field from first data-set
  postcode_one = POSTCODE, # postcode field from first data-set
  df_two = df_B, # second data-set
  id_two = ID, # unique id field from second data-set
  forename_two = FORENAME, # forename field from second data-set
  surname_two = SURNAME, # surname field from second data-set
  dob_two = DOB, # date of birth field from second data-set
  postcode_two = POSTCODE, # postcode field from second data-set
  output_type = "key", # only return the key match results
  format_data = TRUE, # format input data-sets prior to matching
  inc_no_match = TRUE # return records from first data-set without matches
)
```

    ## # A tibble: 4 x 5
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

### Match function parameter: output_type

This parameter will determine the number of fields from each data-set
returned in the output:

- key
  - this option will only return the ID fields from each data-set, plus
    the match outcome and match score
  - this output could then be used to join to original data-sets as
    required
- match
  - this option will return the “key” fields plus the personal
    information fields used in the matching
  - this output would allow the matches to easily be reviewed
- all
  - will return the key fields, plus all other fields from both
    data-sets where a match was found

### Match function parameter: format_data (not used for database match functions)

This parameter will determine whether or not the data is formatted as it
is passed to the matching function. Formatting the data can help ensure
both data-sets are consistently formatted, accounting for things like
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

### Match function parameter: inc_no_match

This parameter will determine whether or not the output results will
include details of records where no match could be found:

- TRUE
  - all records from the initial data-set will be included in the
    output, even if no match was found
- FALSE
  - the output will only include records where a match could be
    identified
