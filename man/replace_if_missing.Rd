% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{replace_if_missing}
\alias{replace_if_missing}
\title{Helper used when putting together the query parameters}
\usage{
replace_if_missing(params, field_and_defaults)
}
\arguments{
\item{params}{list of the parameters that will go into the API request}

\item{field_and_defaults}{list of import params with default values}
}
\value{
modified \code{params} with the defaults fields / values added if they weren't present
}
\description{
Set up default values if not specified by the user
}
\details{
Even if these parameters were omitted it wouldn't change the result, but it's important to have their value be set explicitly so that other parts of the package can know what they are.
}
