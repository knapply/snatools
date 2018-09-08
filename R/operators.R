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
#' 
`%==%` <- function(lhs, rhs) {
  if(class(lhs) %in% c("igraph", "network") && class(rhs) %in% c("igraph", "network")) {
    lhs_guts <- dissect_graph(lhs)
    rhs_guts <- dissect_graph(rhs)
    if(!identical(lhs_guts$net_attrs, rhs_guts$net_attrs, ignore.environment = TRUE)) {
      message("graph-level attributes don't match")
      return(FALSE)
    }
    if(!identical(lhs_guts$edg_attrs, rhs_guts$edg_attrs, ignore.environment = TRUE)) {
      message("edge attributes don't match")
      return(FALSE)
    }
    if(!identical(lhs_guts$vrt_attrs, rhs_guts$vrt_attrs, ignore.environment = TRUE)) {
      message("vertex attributes don't match")
      return(FALSE)
    }
    if(!identical(lhs_guts$el, rhs_guts$el, ignore.environment = TRUE)) {
      message("edge lists don't match")
      return(FALSE)
    }
    return(TRUE)
  }
  identical(lhs, rhs)
}
