[![Travis build status](https://travis-ci.org/dashstander/pushshiftr.svg?branch=master)](https://travis-ci.org/dashstander/pushshiftr)

[![Codecov test coverage](https://codecov.io/gh/dashstander/pushshiftr/branch/master/graph/badge.svg)](https://codecov.io/gh/dashstander/pushshiftr?branch=master)

# pushshiftr

An R package for connection to the https://pushshift.io API.

## Installation

I only just started this, so it's not on [CRAN](https://CRAN.R-project.org) (and I'm not at all sure it ever will be). If you'd like to install this and give it a whirl, the best way would be to use the `devtools` package:

``` r
install.packages("devtools")
devtools::install_github("whereofonecannotspeak/pushshiftr")
```

## Example

The basic use case is searching submissions or comments. If you want to know whether your question about Romance of the Three Kingdoms has been asked on /r/AskHistorians recently you might run this command:

``` r
ps_search_submissions(search_terms = c("Cao Cao", "Liu Bei"), subreddit = "AskHistorians", since = "2017-01-01")
```

