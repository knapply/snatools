# "default"ers =========================================================================


#' @name defaulters
#' 
#' Default Returns
#' 
#' @param .lhs Object to test.
#' @param .rhs Object to return if test returns `FALSE` for `.lhs`.
#' 
#' @details 
#' * Tests:
#'    + `%||%`: `is.null(.lhs)`
#'    + `%{}%`: `length(.lhs) == 0L`
#'    + `%{NA}%`: `is.na(.lhs)`
#' 
#' @export
`%||%` <- function(.lhs, .rhs) {
  if (is.null(.lhs)) .rhs else .lhs
}

#' @rdname defaulters
#' 
#' @export
`%{}%` <- function(.lhs, .rhs) {
  if (length(.lhs) == 0L) .rhs else .lhs
}

#' @rdname defaulters
#' 
#' @export
`%{NA}%` <- function(.lhs, .rhs) {
  if (is.na(.lhs)) .rhs else .lhs
}


# `is_*()` =============================================================================
#' @name tests
#' Testers
#' 
#' @param .x Object to test
#' 
#' @details 
#' * Tests:
#'   + `is_empty()`: `length(.x) == 0L`
#'   
#' @export
is_empty <- function(.x) {
  length(.x) == 0L
}

