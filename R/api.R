library(httr)
library(jsonlite)

BASE_URL <- "https://api.pushshift.io/"
BASE_PATH <- "reddit/search/%s/"
USER_AGENT <- httr::user_agent("www.github.com/whereofonecannotspeak/pushiftr")





#####################################################################
#'
#'
.aggregate_search_terms <- function(search_terms) {

  if (is.na(search_terms)) return(list())

  q_list <- function(x) list(q = x)

  Reduce(c, lapply(search_terms, q_list))
}



#####################################################################
#'
#'
.build_url <- function(path, search_terms, params) {

  url = parse_url(BASE_URL)

  url$path = path
  url$query = c(.aggregate_search_terms(search_terms), params)

  httr::build_url(url)
}



#####################################################################
#'
#'
ps_reddit_api <- function(type, search_terms = NA, ...) {

  assert_that(type %in% c("comment", "submission"))

  params = list(...)

  path = sprintf(BASE_PATH, type)

  url = .build_url(path, search_terms, params)

  response = httr::GET(url, USER_AGENT)


  parsed = jsonlite::fromJSON(content(response,
                                      type = "text",
                                      encoding = "UTF-8"))

  if (http_error(response)) {
    stop(
      sprintf(
        "pushshift.io API request failed [%s]\n%s\n<%s>",
        status_code(response),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed$data,
      path = path,
      response = response
    ),
    class = "pushshift_api"
  )
}
