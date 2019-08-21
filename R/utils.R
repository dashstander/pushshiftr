
#' The opposite of %in%
#'
#' Easy replacement for the \code{!(x %in y)} pattern
#'
#' The implementation of this is just calling \code{Negate} on \code{%in%},
#' so check the documentation on \code{%in%} for more information.
#'
#'
#' @usage x %in% y
#' @param x vector, the values to be (not) matched
#' @param y vector, the values to be (not) matched against
#'
#' @examples
#' 4 %notin% c(1:3, 5:10)
#' "hello" %notin% c("hello", "it's", "me")
#' @export
`%notin%` <- Negate(`%in%`)
