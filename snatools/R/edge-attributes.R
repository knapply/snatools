#' @title Get Edge Attribute Names
#' 
#' @template graph-param
#' 
#' @return `character` `vector` of `x`'s vertex attribute names or `NULL` if
#' no edge attributes exist.
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' 
#' florence %>% 
#'   edg_attr_names()
#' 
#' @export
edg_attr_names <- function(x) {
  UseMethod("edg_attr_names")
}

#' @rdname edg_attr_names
#' 
#' @seealso [igraph::edge_attr_names()]
#' 
#' @importFrom igraph edge_attr_names
#' @export
edg_attr_names.igraph <- function(x) {
  out <- edge_attr_names(x)
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname edg_attr_names
#' 
#' @seealso [network::list.edge.attributes()]
#' 
#' @export
edg_attr_names.network <- function(x) {
  out <- unique(unlist(lapply(lapply(x[["mel"]], `[[`, "atl"), names)))
  out <- out[out != "na"]
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname edg_attr_names
#' 
#' @export
edg_attr_names.tbl_graph <- function(x) {
  edg_attr_names.igraph(as_igraph.tbl_graph(x))
}

#* ====

#' @title Get a Specific Edge Attribute
#' 
#' @template graph-param
#' @template edg_attr-param
#' 
#' @return `vector` containing the vertex attributes specified by `vrt_attr`.
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' 
#' florence %>% 
#'   edg_get_attr("relation")
#' 
#' @export
edg_get_attr <- function(x, edg_attr) {
  UseMethod("edg_get_attr")
}

#' @rdname edg_get_attr
#' 
#' @seealso [igraph::edge_attr()]
#' 
#' @importFrom igraph edge_attr
#' @export
edg_get_attr.igraph <- function(x, edg_attr) {
  validate_args(x = x, edg_attr = edg_attr)
  edge_attr(x, edg_attr)
}

#' @rdname edg_get_attr
#' 
#' @export
edg_get_attr.network <- function(x, edg_attr) {
  validate_args(x = x, edg_attr = edg_attr)
  unlist(lapply(lapply(x[["mel"]], `[[`, "atl"), `[[`, edg_attr %||% NA))
}

#' @rdname edg_get_attr
#' 
#' @export
edg_get_attr.tbl_graph <- function(x, edg_attr) {
  edg_get_attr.igraph(as_igraph.tbl_graph(x), edg_attr)
}







