#' Strict object comparison.
#' 
#' * This infix operator
#'     + provides stricter comparisons than `==`.
#'     + prevents silent type conversions.
#'     + offers a method to easily compare `igraph` objects.
#' 
#' @param lhs left-hand side
#' @param rhs right-hand side
#' 
#' @return `logical` Whether `lhs` and `rhs` are identical.
#' 
#' @name %==%
#' 
#' @rdname strict-compare
#' 
#' @seealso [`base::identical()`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' 
#' TRUE == 1
#' TRUE %==% 1
#' 
#' NA == NA_character_
#' NA %==% NA_character_
#' 
#' NULL == NA_integer_
#' NULL %==% NA_integer_
#' 
#' NA == "NA"
#' NA %==% "NA"
#' 
#' igraph::graph("Zachary") %==% igraph::graph("Zachary")
#' 
#' @export
`%==%` <- function(lhs, rhs) {
  UseMethod("%==%")
}


#' @rdname strict-compare
#' 
#' @export
#' 
`%==%.default` <- function(lhs, rhs) {
  identical(lhs, rhs)
}

#' @rdname strict-compare
#' 
#' @export
#' 
`%==%.igraph` <- function(lhs, rhs) {
  identical(unclass(lhs)[seq_len(9)], unclass(rhs)[seq_len(9)])
}

# %l0%` <- function(x, y) if (length(x) == 0) y else x




