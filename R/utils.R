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
#'   + `%{T}%`: `TRUE`
#'   + `%{F}%`: `FALSE`
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
#' 
#' TRUE %{T}% FALSE
#' FALSE %{T}% TRUE
#' 
#' TRUE %{F}% FALSE
#' FALSE %{F}% TRUE

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

#' @rdname defaulters
#' 
#' @export
`%{T}%` <- function(.lhs, .rhs) {
  if (isTRUE(.lhs)) .rhs else .lhs
}

#' @rdname defaulters
#' 
#' @export
`%{F}%` <- function(.lhs, .rhs) {
  if (.isFALSE(.lhs)) .rhs else .lhs
}


# `is_*()`/`all_*()` ====================================================================

all_equal <- function(...) {
  objs <- list(...)
  stopifnot(length(objs) > 1L)

  all(
    vapply(objs[2L:length(objs)], function(x) {
      isTRUE(all.equal(objs[[1L]], x))
    }, logical(1L))
  )
}

all_identical <- function(...) {
  objs <- list(...)
  stopifnot(length(objs) > 1L)
  
  all(
    vapply(objs[2L:length(objs)], function(x) {
      identical(objs[[1L]], x)
    }, logical(1L))
  )
}


is_empty <- function(.x) {
  length(.x) == 0L
}

is_symmetric <- function(.x) {
  stopifnot(is.matrix(.x))
  all(upper.tri(.x) == lower.tri(.x))
}

.isFALSE <- function(x) { 
  # `isFALSE()` apparely didn't show up until R 3.5
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
