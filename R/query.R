
fields_with_defaults <- list("size" = 25, sort = "desc", sort_type = "created_utc")

comma_sep_fields <- c("ids", "fields", "author", "subreddit")
aggregate_fields <- c("author", "link_id", "created_utc", "subreddit")


#####################################################################
#' Creates a Query S3 Object from many distinct pieces.
#' @description
#' Pass in the type of query and all the other varied arguments that you need and it will
#'   create a proper ps_query object for you.
#' @param type the type of query (subreddit, post, comment, etc...)
#' @param \dots all of the other arguments
#' @export
ps_query <- function(type, ...) {
  query_pieces <- .build_query(type, ...)

  do.call(new_ps_query, query_pieces)
}



#####################################################################
#' Structured way to create to properly create a ps_query object
#' @param url the full URL endpoint the query is going to hit (params and all)
#' @param type the type of query
#' @param sort_type whether to sort on time, score, etc...
#' @param sort_direction ascending or descending
#' @param pagination_strategy different ways to paginate
#' @param size the number of rows returned per request
#' @export
new_ps_query <- function(url, type, sort_type, sort_direction, pagination_strategy, size) {
  structure(
    list(url = url,
         type = type,
         sort_type = sort_type,
         sort_direction = sort_direction,
         pagination_strategy = pagination_strategy,
         size=size),
    class =  "ps_query"
  )
}


#####################################################################
#' Helper function that puts together all the necessary parameters
#' @param type the type of query
#' @param search_terms character vector of key words to search for
#' @param \dots all of the other pieces of the query
#' @export
.build_query <- function(type, search_terms, ...) {

  stopifnot(type %in% c("comment", "submission", "subreddit"))

  path <- sprintf(BASE_PATH, type)

  params <- .build_params(...)

  url <- .build_url(path, params)

  if (any(names(params) %in% comma_sep_fields)) url <- .replace_encoded_commas(url)


  sort_type <- params[["sort_type"]]
  sort_direction <- params[["sort"]]

  pagination_strategy <- ifelse("ids" %in% names(params), "fixed", "incremental")


  list(url = url,
       type = type,
       sort_type = sort_type,
       sort_direction = sort_direction,
       pagination_strategy = pagination_strategy,
       size = params[["size"]])

}



#####################################################################
#' Uses parameters to build the URL the request will hit
#' @param path the path of the query
#' @param params named list of all of the other parameters (key words, size, etc...)
#' @export
.build_url <- function(path, params) {

  url <- httr::parse_url(BASE_URL)

  url$path <- path
  url$query <- params

  httr::build_url(url)
}


#####################################################################
#' Ensures all parameters are in the proper format
#' @description Makes sure all necessary params are there and all vector params are comma-separated
#' @param \dots Whatever params are given
#' @return all of the necessary parameters
#' @export
.build_params <- function(...) {
  params <- list(...)

  if ("q" %in% names(params)) params[["q"]] <- .parse_search_terms(params[["q"]])

  params <- replace_if_missing(params, fields_with_defaults)

  vec_params = sapply(params, function(x) length(x) > 1)
  params[vec_params] = param_to_csv(params[vec_params])

  params
}


#####################################################################
#' Makes sure search terms are in the proper format
#' @description Search terms that have spaces or commas are quotes
#' @param search_terms character vector of the search terms
#' @return correctly formatted search terms
#' @export
.parse_search_terms <- function(search_terms) {

  has_comma_or_space <- grepl(",| ", search_terms)

  search_terms[has_comma_or_space] <- paste0("'", search_terms[has_comma_or_space], "'")

  param_to_csv(search_terms)
}


#####################################################################
#' Remove url encoded commas
#' @description currently URL encoded commas are not supported by api.pushshift.io but
#'   are mandatory for httr. This replaces them with normal commas at the very end.
#' @return the url without URL encoded commas
#' @noRd
.replace_encoded_commas <- function(url) {

  field_match_pattern <- paste0("^", paste(comma_sep_fields, collapse = "|"), "=.*")

  # Main api path (e.g. api.pushshift.io/search/submission/) separated from query params
  # by a "?"
  query_split <- stringr::str_split(url, "\\?")[[1]]

  # Individual query params separated by a "&"
  param_split <- stringr::str_split(query_split[[2]], "&")[[1]]

  comma_sep_params <- stringr::str_detect(string = param_split,
                                         pattern = field_match_pattern)

  param_split[comma_sep_params] <- stringr::str_replace_all(param_split[comma_sep_params],
                                                           pattern = "%2C",
                                                           replacement = ",")

  rebuilt_url <- paste0(query_split[[1]], "?",
                       paste(param_split, collapse = "&"))

  rebuilt_url
}

