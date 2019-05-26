
#' This is my favorite function.
#' @export
`%notin%` <- Negate(`%in%`)


replace_if_missing <- function(x, field_and_defaults) {

  missing_fields <- names(field_and_defaults)[names(field_and_defaults) %notin% names(x)]

  x[missing_fields] <- field_and_defaults[missing_fields]

  x
}


param_to_csv <- function(value) {

  value <- purrr::modify(value, as.character)

  if (is.list(value)) {
    csv_value <- purrr::map(value, ~paste(.x, collapse = ","))
  } else {
    csv_value <- paste(value, collapse = ",")
  }

  #lapply(lapply(value, as.character), function(x) paste(x, collapse = ","))

  csv_value
}
