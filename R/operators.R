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
  if(class(rhs) != "igraph") {
    stop("`lhs` and `rhs` are not both `igraph` objects.")
  }
  lhs_net_attrs <- net_get_attrs(lhs)
  lhs_net_attrs <- lhs_net_attrs[order(names(lhs_net_attrs))]
  rhs_net_attrs <- net_get_attrs(rhs)
  rhs_net_attrs <- rhs_net_attrs[order(names(rhs_net_attrs))]
  for(i in names(lhs_net_attrs)) {
    lhs <- igraph::delete_graph_attr(lhs, i)
  }
  for(i in names(rhs_net_attrs)) {
    rhs <- igraph::delete_graph_attr(rhs, i)
  }
  tests <- c(identical(lhs_net_attrs, rhs_net_attrs),
             identical(unclass(lhs)[seq_len(9)], unclass(rhs)[seq_len(9)]))
  
  all(tests)
}


#' @rdname strict-compare
#' 
#' @export
#' 
`%==%.network` <- function(lhs, rhs) {
  if(class(rhs) != "network") {
    stop("`lhs` and `rhs` are not both `network` objects.")
  }

  lhs <- clean_network_metadata(lhs)
  rhs <- clean_network_metadata(rhs)
  
  identical(lhs, rhs)
}
