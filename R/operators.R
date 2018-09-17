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

#' @rdname %==%
#' 
#' @export
`%==%.default` <- function(lhs, rhs) {
  identical(lhs, rhs)
}

#' @rdname %==%
#' 
#' @export
`%==%.igraph` <- function(lhs, rhs) {
  lhs <- as_sna_net(lhs)
  lhs[["graph_attributes"]] <- NULL
  rhs <- as_sna_net(rhs)
  rhs[["graph_attributes"]] <- NULL
  identical(lhs, rhs)
}

#' @rdname %==%
#' 
#' @export
`%==%.network` <- function(lhs, rhs) {
  lhs <- as_sna_net(lhs)
  lhs[["graph_attributes"]] <- NULL
  rhs <- as_sna_net(rhs)
  rhs[["graph_attributes"]] <- NULL
  identical(lhs, rhs)
}

#' @rdname %==%
#' 
#' @export
`%==%.tbl_graph` <- function(lhs, rhs) {
  identical(as_sna_net(lhs), as_sna_net(rhs))
}


`%||%` <- function(lhs, rhs) {
  if (identical(lhs, NULL)) return(rhs)
  lhs
}

`%{}%` <- function(lhs, rhs) {
  if (length(lhs) == 0L) return(rhs)
  lhs
}