% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_dob.R
\name{filter_dob}
\alias{filter_dob}
\title{Filter data on date of birth to limit cross-join}
\usage{
filter_dob(df, dob_one, dob_two, dob_score_threshold)
}
\arguments{
\item{df}{a dataframe to feed into function}

\item{dob_one}{the field containing the first date of birth column}

\item{dob_two}{the field containing the second date of birth column}

\item{dob_score_threshold}{allows the match score threshold to be adjusted}
}
\value{
A df filtered to only include suitable combinations of date of birth
}
\description{
This is a support function called during the execution of the calc_match_person function.
\cr\cr Applies a filter on date of birth to limit volume of data from cross-join.
\cr\cr Limits cross-joins to only include combinations of date of birth where the two dates
have a minunmum of 6 characters in common. This is similar to an edit distance of 2 on a 8 digit
string using Levenshtein distance algorithm.
\cr\cr This will prevent any records being considered for matching where the date of births are
notably different which would mean no match would be expected.
}
