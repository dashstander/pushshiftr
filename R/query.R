
fields_with_defaults <- list("size" = 25, sort = "desc", sort_type = "created_utc")

comma_sep_fields <- c("ids", "fields", "author", "subreddit")
aggregate_fields <- c("author", "link_id", "created_utc", "subreddit")


#####################################################################
#'
#' @export
ps_query <- function(type, ...) {
  query_pieces <- .build_query(type, ...)

  do.call(new_ps_query, query_pieces)
}



#####################################################################
#'
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
#'
#' @export
.build_url <- function(path, params) {

  url <- httr::parse_url(BASE_URL)

  url$path <- path
  url$query <- params

  httr::build_url(url)
}


#####################################################################
#'
#' @export
.build_params <- function(...) {
  params <- list(...)

  if ("q" %in% names(params)) params[["q"]] <- .parse_search_terms(params[["q"]])

  params <- replace_if_missing(params, fields_with_defaults)

  vec_params = sapply(params, function(x) length(x) > 1)

  print(params[vec_params])

  params[vec_params] = param_to_csv(params[vec_params])

  params
}


#####################################################################
#'
#' @export
.parse_search_terms <- function(search_terms) {

  has_comma_or_space <- grepl(",| ", search_terms)

  search_terms[has_comma_or_space] <- paste0("'", search_terms[has_comma_or_space], "'")

  param_to_csv(search_terms)
}


#####################################################################
#'
#' @export
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

