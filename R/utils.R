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
.isFALSE <- function(x) { 
  # `isFALSE()` apparely didn't show up until R 3.5
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

# `%==%` <- function(.lhs, .rhs) {
#   isTRUE(all.equal(.lhs, .rhs))
# }
# 
# `%!=%` <- function(.lhs, .rhs) {
#   !isTRUE(all.equal(.lhs, .rhs))
# }

.all_equal <- function(...) {
  objs <- list(...)
  stopifnot(length(objs) > 1L)
  
  if (length(objs) == 2L) {
    return(isTRUE(all.equal(objs[[1L]], objs[[2L]])))
  }
  
  for (i in seq_along(objs[2L:length(objs)])) {
    if (!isTRUE(all.equal(objs[[1L]], objs[[i]]))) {
      return(FALSE)
    }
  }
  TRUE
}

.all_identical <- function(...) {
  objs <- list(...)
  stopifnot(length(objs) > 1L)
  
  if (length(objs) == 2L) {
    return(identical(objs[[1L]], objs[[2L]]))
  }
  
  for (i in seq_along(objs[2L:length(objs)])) {
    if (!identical(objs[[1L]], objs[[i]])) {
      return(FALSE)
    }
  }
  TRUE
}

.tri_upper <- function(.x, .diag = FALSE) {
  stopifnot(is.matrix(.x))
  .x[upper.tri(.x, diag = .diag)]
}

.tri_lower <- function(.x, .diag = FALSE) {
  stopifnot(is.matrix(.x))
  .x[lower.tri(.x, diag = .diag)]
}

.is_empty <- function(.x) {
  length(.x) == 0L
}

.is_symmetric <- function(.x, .diag = FALSE) {
  stopifnot(is.matrix(.x))
  all(
    .tri_upper(.x, .diag = .diag) == .tri_lower(.x, .diag = .diag)
  )
}


