# "default"ers =========================================================================

#' @name defaulters
#' 
#' @title Conditionally replace objects with default values.
#' 
#' @param .lhs Object to test.
#' @param .rhs Object to return if test returns `FALSE` for `.lhs`.
#' 
#' @return `.lhs` or `.rhs`
#' 
#' @template author-bk
#' 
#' @details 
#' * If `.lhs` is...
#'   + `%||%`: `NULL`
#'   + `%{}%`: empty (`length(.lhs) == 0L`)
#'   + `%{NA}%`: `NA`
#' * ... returns `.rhs`.
#'   + Returns `.lhs` otherwise.
#' 
#' @examples 
#' NULL %||% 1
#' 1 %||% NULL
#' 
#' integer(length = 0L) %{}% NA
#' 1 %{}% NA
#' 
#' NA %{NA}% 1
#' 1 %{NA}% NA

#' @rdname defaulters
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
  if (isTRUE(is.na(.lhs))) .rhs else .lhs
}


# `is_*()` =============================================================================

is_empty <- function(.x) {
  length(.x) == 0L
}

