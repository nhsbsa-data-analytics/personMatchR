% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_name.R
\name{filter_name}
\alias{filter_name}
\title{Filter data on name to limit cross-join}
\usage{
filter_name(df, name_one, name_two)
}
\arguments{
\item{df}{a dataframe to feed into function}

\item{name_one}{the field containing the first name column}

\item{name_two}{the field containing the second name column}
}
\value{
A df filtered to only include suitable combinations of names
}
\description{
This is a support function called during the execution of the calc_match_person function.
\cr\cr Applies a filter on name to limit volume of data from cross-join.
\cr\cr Limits cross-joins to only include name-pair instances that share certain characteristics.
This includes cases where the two names contain the same 1st, 2nd or last letter, or being a
sub-string of the other name.
\cr\cr This will prevent any records being considered for matching where the names are notably
different which would mean no match would be expected.
}
