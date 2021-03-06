
BASE_URL <- "https://api.pushshift.io/"
BASE_PATH <- "reddit/search/%s/"
USER_AGENT <- httr::user_agent("www.github.com/dashstander/pushshiftr")


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

  parsed <- jsonlite::fromJSON(
    httr::content(
      response,
      type = "text",
      encoding = "UTF-8"
    ),
    simplifyVector = FALSE
  )

  data <- purrr::keep(parsed$data, ~.x$id %notin% provided_ids)

  all_utc <- purrr::map_int(data, ~.x$created_utc)

  structure(
    list(
      min_utc = tryCatch(min(all_utc),
                         warning = function(e) NA),
      max_utc = tryCatch(max(all_utc),
                         warning = function(e) NA),
      content = data,
      header = httr::headers(response)
    ),
    class = "ps_api_response"
  )
}




#####################################################################
#' Using the API for search
#' @description Builds the query from parameters that you pass in, handles pagination, etc...
#' @param type Either "comment" or "submission", depending on which objects you want to search.
#' @param \dots All of the other search parameters (e.g. size, before, until, etc...)
#' @return a dataframe of the objects that the API has returned
#' @export
ps_search <- function(type = c("comment", "submission", "subreddit"), ...) {
  type <- match.arg(type)

  query <- ps_query(type=type, ...)

  size <- query$size

  data <- list()
  ids <- c()
  i <- 1

  while(TRUE) {
    response <- ps_reddit_api(query$url, ids)
    ids <- c()

    if (is.list(response$content) && length(response$content)) data[[i]] <- response$content

    if (length(response$content) > 0) {
      print(
        sprintf(
          "Found %d rows, with min_time of %s",
          length(response$content),
          as.character(
            lubridate::as_datetime(response$min_utc)
          )
        )
      )
      query$url <- httr::modify_url(query$url, query = list(until = response$min_utc + 1))
      ids <- .get_possible_duplicate_ids(response$content, response$min_utc)
      i <- i + 1
      Sys.sleep(0.5)
    } else {
      break
    }
  }
  purrr::flatten(data)
}

#####################################################################
#'Search submissions
#' @description High level function that lets you easily search submissions.
#' @param search_terms The terms to search for. Will search the post title and selftext.
#' @param \dots Whatever other query parameters that are provided (e.g. size, before, until, etc...)
#' @return a dataframe
#' @export
ps_search_submissions <- function(search_terms = "", ...) {

  ps_search(type = "submission", ...)

}



#####################################################################
#'Search reddit comments
#' @description High level function that lets you easily search comments
#' @param search_terms character vector of the key words to search for
#' @param ... Any extra parameters to give the query, e.g. since, until, etc...
#' @export
ps_search_comments <- function(search_terms = "", ...) {

  ps_search(type = "comment", ...)

}



#####################################################################
#'Search reddit comments
#' @description Gets the comment ids for the comments in specific posts
#' @param submission_id the id of the submission (post) in question.
#' @export
ps_get_comment_ids <- function(submission_id) {

}



#####################################################################
#'Aggregate reddit posts
ps_agg_submissions <- function(
  agg_type=c("author", "link_id", "created_utc", "subreddit"),
  frequency=c("second", "minute", "hour", "day"), ...) {


}


