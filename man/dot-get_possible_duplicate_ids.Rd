% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.get_possible_duplicate_ids}
\alias{.get_possible_duplicate_ids}
\title{Helper used in pagination.}
\usage{
.get_possible_duplicate_ids(data, min_utc)
}
\arguments{
\item{data}{a dataframe of the data returned by the API request}

\item{min_utc}{the minimum created_utc value from the request}
}
\value{
the ids that may be duplicated in the next request
}
\description{
Returns object ids that may be duplicated in the next request
}
