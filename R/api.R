library(httr)
library(jsonlite)
library(lubridate)

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
.build_url <- function(path, params) {

  url = httr::parse_url(BASE_URL)

  url$path = path
  url$query = params

  httr::build_url(url)
}


#####################################################################
#'
#'
parse_dates <- function(params) {
  until = params[["until"]]
  since = params[["since"]]


  if (!is.null(until) && !is.numeric(until)) {
    params[["until"]] = lubridate::seconds(lubridate::as_datetime(until))
  }

  if (!is.null(since) && !is.numeric(since)) {
    params[["since"]] = lubridate::seconds(lubridate::as_datetime(since))
  }

  params
}


parse_search_terms <- function(params) {
  search_terms = params[names(params) == ""]
  other_params = params[names(params) != ""]

  has_comma_or_space = grepl(",| ", search_terms)

  search_terms[has_comma_or_space] = paste0("'", search_terms[has_comma_or_space], "'")

  other_params[["q"]] = paste(search_terms, collapse = ",")
  other_params
}


#####################################################################
#'
#'
.build_params <- function(...) {
  params = parse_search_terms(list(...))

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
#' @description Given a URL, returns the response
#' @param url httr URL object, contains all of the info about which endpoint, search terms, etc...
#' @param provided_ids character vector of ids to exclude
#' @export
ps_reddit_api <- function(url, provided_ids = character(0)) {

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

  Data = parsed$data[parsed$data$id %notin% provided_ids, ]

  structure(
    list(
      min_utc = tryCatch(min(Data$created_utc),
                         warning = function(e) NA),
      max_utc = tryCatch(max(Data$created_utc),
                         warning = function(e) NA),
      content = Data,
      header = httr::headers(response)
    ),
    class = "pushshift_api"
  )
}




#####################################################################
#' Using the API for search
#' @description Builds the query from parameters that you pass in, handles pagination, etc...
#' @param type Either "comment" or "submission", depending on which objects you want to search.
#' @return a dataframe of the objects that the API has returned
#' @export
ps_search <- function(type = c("comment", "submission"), ...) {
  type = match.arg(type)

  url = .build_query(type, ...)

  size = httr::parse_url(url)$query$size

  data = list()
  ids = c()
  i = 1
  while(TRUE) {
    #if (i > 10) break
    print(url)
    response = ps_reddit_api(url, ids)
    ids = c()

    if (is.data.frame(response$content) && nrow(response$content)) data[[i]] = response$content

    if (nrow(response$content) > 0) {
      print(sprintf("Found %d rows, with min_time of %s",
                    nrow(response$content),
                    as.character(lubridate::as_datetime(response$min_utc))))
      url = httr::modify_url(url, query = list(until = response$min_utc + 1))
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
#' @description High level function that lets you easily search submissions.
#' @param search_terms
#' @export
search_submissions <- function(...) {

  ps_search(type = "submission", ...)

}



#####################################################################
#'Search reddit comments
#' @description High level function that lets you easily search comments
#' @param search_terms
#' @export
search_comments <- function(...) {

  ps_search(type = "comment", ...)

}
