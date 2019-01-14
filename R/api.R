library(httr)
library(jsonlite)

BASE_URL <- "https://api.pushshift.io/"
BASE_PATH <- "reddit/search/%s/"
USER_AGENT <- httr::user_agent("www.github.com/whereofonecannotspeak/pushiftr")





#####################################################################
#' Create list of search terms
#' Deprecated
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
  url$query = params

  httr::build_url(url)
}



#####################################################################
#'
#'
.build_params <- function(...) {
  params = list(...)

  names(params)[names(params) == ""] = "q"
  if (is.null(params[["size"]])) params[["size"]] = 25
  if (is.null(params[["sort"]])) params[["sort"]] = "desc"

  params
}



#####################################################################
#'
.build_query <- function(type, ...) {

  assert_that(type %in% c("comment", "submission"))

  path = sprintf(BASE_PATH, type)

  params = .build_params(...)

  url = .build_url(path, search_terms, params)
}



#####################################################################
#'
.get_possible_duplicate_ids <- function(data, min_utc) {

  data[data$created_utc <= min_utc + 1, "id"]

}



#####################################################################
#' Basic function to query the pushshift.io API
#' @description
#' @export
ps_reddit_api <- function(url, provided_ids = character(0)) {

  response = httr::GET(url, USER_AGENT)

  if (http_error(response)) {
    stop(
      sprintf(
        "pushshift.io API request failed [%s]",
        status_code(response)
      ),
      call. = FALSE
    )
  }

  parsed = jsonlite::fromJSON(content(response,
                                      type = "text",
                                      encoding = "UTF-8"),
                              simplifyDataFrame = TRUE)

  Data = parsed$data[parsed$data$id %notin% provided_ids, ]

  structure(
    list(
      min_utc = min(Data$created_utc),
      max_utc = max(Data$created_utc),
      content = Data,
      header = headers(response)
    ),
    class = "pushshift_api"
  )
}


#####################################################################
#'Search submissions
#' @description High level function that lets you easily search submissions.
#' @param search_terms
#' @export
search_submissions <- function(...) {

  url = .build_query("submission", ...)

  size = httr::parse_url(url)$query$size

  data = list()
  ids = c()
  i = 1
  while(TRUE) {
    response = ps_reddit_api(url, ids)
    data[[i]] = response$content

    if (nrow(response$content) == size) {
      url = httr::modify_url(url, query = list(after = response$min_utc + 1))
      ids = .get_possible_duplicate_ids(response$content, response$min_utc)
      i = i + 1
      Sys.sleep(1)
    } else {
      break
    }
  }

  jsonlite::rbind_pages(data)
}
