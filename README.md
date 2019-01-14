[![Travis build status](https://travis-ci.org/whereofonecannotspeak/pushshiftr.svg?branch=master)](https://travis-ci.org/whereofonecannotspeak/pushshiftr)

# pushshiftr

An R package for connection to the https://pushshift.io API.

## Installation

I only just started this, so it's not on [CRAN](https://CRAN.R-project.org) (and I'm not at all sure it ever will be). If you'd like to install this and give it a whirl, the best way would be to use the `devtools` package:

``` r
devtools::install_github("whereofonecannotspeak/pushshiftr")
```

## Example

The basic use case is searching submissions or comments. If you want to know whether your question about Romance of the Three Kingdoms has been asked on /r/AskHistorians recently you might run this command:

``` r
search_submissions("Cao Cao", "Liu Bei", subreddit = "AskHistorians", after = "30d")
```

