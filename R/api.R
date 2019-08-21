library(httr)
library(jsonlite)

BASE_URL <- "https://api.pushshift.io/"
BASE_PATH <- "reddit/search/%s/"
USER_AGENT <- httr::user_agent("www.github.com/whereofonecannotspeak/pushiftr")





#####################################################################
#' Create list of search terms
#' Deprecated
#' @description Aggregate vector of search terms to build the API call
#' @param search_terms a character vector of search terms that will be matched against the text of the submissions or comments.
#' @return a list of all of the given search terms, with 'q' as the name for each of them
.aggregate_search_terms <- function(search_terms) {

  if (is.na(search_terms)) return(list())

  q_list <- function(x) list(q = x)

  Reduce(c, lapply(search_terms, q_list))
}



#####################################################################
#' Put together the pieces of the URL
#' @description
.build_url <- function(path, params) {

  url = httr::parse_url(BASE_URL)

  url$path = path
  url$query = params

  httr::build_url(url)
}



#####################################################################
#' Builds the list of query parameters
#' @description Puts together the list of query parameters necessary to query the API.
#' @param ... the list of parameters. Non-named parameters are assumed to be search terms.
#' @return list of parameters
#' @details There are two necessary parameters: "size" and "sort". If they are not present, then they
#' are added. Their default values are 25 and "desc" (i.e. "descending"), respectively.
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

  stopifnot(type %in% c("comment", "submission"))

  path = sprintf(BASE_PATH, type)

  params = .build_params(...)

  url = .build_url(path, params)
}



#####################################################################
#'
.get_possible_duplicate_ids <- function(data, min_utc) {

  data[data$created_utc <= min_utc + 1, "id"]

}



#####################################################################
#' Basic function to query the pushshift.io API
#' @description Hits the API, parses the result, returns the result.
#' @param url: the pushshift.io URL that contains the query
#' @param previously_provided_ids: IDs of comments/submissions that have already been returned
#' @return pushshift_api object, includes the response headers, the content as a dataframe, and the min/max utc of the objects in the response.
#' @details `previously_provided_ids` is a vector of IDs that will be filtered out of the result, if present.
#' Pagination requires that you keep track of the `created_utc` values of the objects you've returned and that you use the
#' `before` and `after` parameters to then cycle through the remaining comments/submissions. There are inevitably some duplicates that need to be dealt with.
#' @export
ps_reddit_api <- function(url, previously_provided_ids = character(0)) {

  response = httr::GET(url, USER_AGENT)

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "pushshift.io API request failed [%s]",
        httr::status_code(response)
      ),
      call. = FALSE
    )
  }

  parsed = jsonlite::fromJSON(httr::content(response,
                                      type = "text",
                                      encoding = "UTF-8"),
                              simplifyDataFrame = TRUE)

  Data = parsed$data[parsed$data$id %notin% previously_provided_ids, ]

  structure(
    list(
      min_utc = min(Data$created_utc),
      max_utc = max(Data$created_utc),
      content = Data,
      header = httr::headers(response)
    ),
    class = "pushshift_api"
  )
}


#####################################################################
#' Search reddit
#'
#' High level function that lets you easily search submissions or comments.
#'
#' @param content Character. Either "comments" or "submissions"
#' @param ... The parameters to the search. Unnamed parameters are assumed to be search terms.
#' @return A dataframe of the results.
search_reddit <- function(content, ...) {

  url = .build_query(content, ...)

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



#####################################################################
#'Search submissions
#' @describeIn search_reddit
#' @export
search_submissions <- function(...) {
  search_reddit("submissions", ...)
}



#####################################################################
#' Search comments
#' @describeIn search_reddit
search_comments <- function(...) {
  search_reddit("comments", ...)
}

