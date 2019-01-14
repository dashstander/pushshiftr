library(httr)
library(jsonlite)

BASE_URL <- "https://api.pushshift.io/"
BASE_PATH <- "reddit/search/%s/"
USER_AGENT <- httr::user_agent("www.github.com/whereofonecannotspeak/pushiftr")





#####################################################################
#' Create list of search terms
#' @description Aggregate vector of search terms to build the API call
#' @arg search_terms a character vector of search terms that will be matched against the text of the submissions or comments.
#' @return a list of all of the given search terms, with 'q' as the name for each of them
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
.build_params <- function(...) {
  params = list(...)
  if (is.null(params[["size"]])) params[["size"]] = 25
  if (is.null(params[["sort"]])) params[["sort"]] = "desc"

  params
}




.build_query <- function(type, search_terms, params, ...) {

  assert_that(type %in% c("comment", "submission"))

  path = sprintf(BASE_PATH, type)

  params = .build_params(...)

  url = .build_url(path, search_terms, params)
}


#####################################################################
#' Basic function to query the pushshift.io API
#' @description
#' @export
ps_reddit_api <- function(url, provided_ids = character(0)) {

  response = httr::GET(url, USER_AGENT)


  parsed = jsonlite::fromJSON(content(response,
                                      type = "text",
                                      encoding = "UTF-8"),
                              simplifyDataFrame = FALSE)

  data = Filter(function(x) x[["id"]] %notin% provided_ids,
                parsed[["data"]])

  if (http_error(response)) {
    stop(
      sprintf(
        "pushshift.io API request failed [%s]",
        status_code(response)
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      min_utc = min(parsed$data$created_utc),
      max_utc = max(parsed$data$created_utc),
      content = data,
      header = headers(response)
    ),
    class = "pushshift_api"
  )
}


#####################################################################
#'
#' @export
search_submissions <- function(search_terms = NA, ...) {

  url = .build_query("submission", search_terms, ...)

  size = httr::parse_url(url)$query$size

  data = list()
  i = 1
  while(TRUE) {
    response = ps_reddit_api(url)
    data[[i]] = response$content

    if (nrow(response$content) == size) {

    }
  }

  hit_endpoint <- function()
}
