#####################################################################
#' This is my favorite function.
#' @description The exact opposite of `%in%``
#' @param ... two vectors: one that you're checking the values for and one that you're checking against
#' @details equivalent to `!x %in% y`
#'
`%notin%` <- Negate(`%in%`)



#####################################################################
#' Helper used in pagination.
#' @description Returns object ids that may be duplicated in the next request
#' @param data a dataframe of the data returned by the API request
#' @param min_utc the minimum created_utc value from the request
#' @return  the ids that may be duplicated in the next request
.get_possible_duplicate_ids <- function(data, min_utc) {

  data[data$created_utc <= min_utc + 1, "id"]

}



#####################################################################
#' Helper used when putting together the query parameters
#' @description Set up default values if not specified by the user
#' @param params list of the parameters that will go into the API request
#' @param field_and_defaults list of import params with default values
#' @details Even if these parameters were omitted it wouldn't change the result, but it's important to have their value be set explicitly so that other parts of the package can know what they are.
#' @return modified `params` with the defaults fields / values added if they weren't present
replace_if_missing <- function(params, field_and_defaults) {

  missing_fields <- names(field_and_defaults)[names(field_and_defaults) %notin% names(params)]

  params[missing_fields] <- field_and_defaults[missing_fields]

  params
}



#####################################################################
#' Turn character vectors into one comma separated string
#' @param value either a character vector or list of character vectors
#' @return value modified such that each individual character vector is now one string with comma separated values
param_to_csv <- function(value) {

  value <- purrr::modify(value, as.character)

  if (is.list(value)) {
    csv_value <- purrr::map(value, ~paste(.x, collapse = ","))
  } else {
    csv_value <- paste(value, collapse = ",")
  }

  csv_value
}
