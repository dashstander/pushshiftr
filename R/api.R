
BASE_URL <- "https://api.pushshift.io/"
BASE_PATH <- "reddit/search/%s/"
USER_AGENT <- httr::user_agent("www.github.com/dashstander/pushshiftr")


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

  response <- httr::GET(url, USER_AGENT)

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "pushshift.io API request failed [%s]",
        httr::status_code(response)
      ),
      call. = FALSE
    )
  }

  parsed <- jsonlite::fromJSON(httr::content(response,
                                             type = "text",
                                             encoding = "UTF-8"),
                              simplifyDataFrame = TRUE)

  Data <- parsed$data[parsed$data$id %notin% provided_ids, ]

  structure(
    list(
      min_utc = tryCatch(min(Data$created_utc),
                         warning = function(e) NA),
      max_utc = tryCatch(max(Data$created_utc),
                         warning = function(e) NA),
      content = Data,
      header = httr::headers(response)
    ),
    class = "ps_api_response"
  )
}




#####################################################################
#' Using the API for search
#' @description Builds the query from parameters that you pass in, handles pagination, etc...
#' @param type Either "comment" or "submission", depending on which objects you want to search.
#' @return a dataframe of the objects that the API has returned
#' @export
ps_search <- function(type = c("comment", "submission", "subreddit"), ...) {
  type <- match.arg(type)

  url <- .build_query(type, ...)

  size <- httr::parse_url(url)$query$size

  data <- list()
  ids <- c()
  i <- 1

  while(TRUE) {
    print(url)
    response <- ps_reddit_api(url, ids)
    ids <- c()

    if (is.data.frame(response$content) && nrow(response$content)) data[[i]] <- response$content

    if (nrow(response$content) > 0) {
      print(sprintf("Found %d rows, with min_time of %s",
                    nrow(response$content),
                    as.character(lubridate::as_datetime(response$min_utc))))
      url <- httr::modify_url(url, query = list(until = response$min_utc + 1))
      ids <- .get_possible_duplicate_ids(response$content, response$min_utc)
      i <- i + 1
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
ps_search_submissions <- function(search_terms = "", ...) {

  ps_search(type = "submission", ...)

}



#####################################################################
#'Search reddit comments
#' @description High level function that lets you easily search comments
#' @param search_terms
#' @export
ps_search_comments <- function(search_terms = "", ...) {

  ps_search(type = "comment", ...)

}



#####################################################################
#'Search reddit comments
#' @description Gets the comment ids for the comments in specific posts
#' @param search_terms
#' @export
ps_get_comment_ids <- function(submission_id) {

}
