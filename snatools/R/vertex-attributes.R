#' @title Get Vertex Attribute Names
#' 
#' @template graph-param
#' 
#' @return `character` `vector` of `x`'s vertex attribute names or  `NULL` if
#' no vertex attributes exist.
#' 
#' @template bknapp-author
#' 
#' @export
vrt_attr_names <- function(x) {
  UseMethod("vrt_attr_names")
}

#' @rdname vrt_attr_names
#' 
#' @seealso [igraph::vertex_attr_names()]
#' 
#' @importFrom igraph vertex_attr_names
#' @export
vrt_attr_names.igraph <- function(x) {
  out <- vertex_attr_names(x)
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname vrt_attr_names
#' 
#' @seealso [network::list.vertex.attributes()]
#' 
#' @export
vrt_attr_names.network <- function(x) {
  out <- unique(unlist(lapply(x[["val"]], names)))
  out <- out[!out == "na"]
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname vrt_attr_names
#' 
#' @export
vrt_attr_names.tbl_graph <- function(x) {
  vrt_attr_names.igraph(as_igraph.tbl_graph(x))
}

#* ====

#' @title Get a Vertex Attribute
#' 
#' @template graph-param
#' @template vrt_attr-param
#' 
#' @return `vector` containing the vertex attributes specified by `vrt_attr`.
#' 
#' @template bknapp-author
#' 
#' @export
vrt_get_attr <- function(x, vrt_attr) {
  UseMethod("vrt_get_attr")
}

#' @rdname vrt_get_attr
#' 
#' @importFrom igraph vertex_attr
#' @export
vrt_get_attr.igraph <- function(x, vrt_attr) {
  validate_args(x = x, vrt_attr = vrt_attr)
  vertex_attr(x, vrt_attr)
}

#' @rdname vrt_get_attr
#' 
#' @export
vrt_get_attr.network <- function(x, vrt_attr) {
  validate_args(x = x, vrt_attr = vrt_attr)
  unlist(lapply(lapply(x[["val"]], `[[`, vrt_attr), `%||%`, NA))
}

#' @rdname vrt_get_attr
#' 
#' @export
vrt_get_attr.tbl_graph <- function(x, vrt_attr) {
  validate_args(x = x, vrt_attr = vrt_attr)
  vrt_get_attr.igraph(as_igraph.tbl_graph(x), vrt_attr)
}

#* ====

#' @title Get Vertex Names
#' 
#' @template graph-param
#' 
#' @return `vector` containing to the values of the vertex attribute that `x`'s class uses
#' for vertex names.
#' 
#' @details
#' Supported graph objects use the following conventions: \cr
#' * `igraph` and `tbl_graph` objects use `name` for vertex names.
#' * `network` objects use `vertex.names` for vertex names.
#' 
#' @template bknapp-author
#' 
#' @export
vrt_get_names <- function(x) {
  UseMethod("vrt_get_names")
}

#' @rdname vrt_get_names
#' 
#' @importFrom igraph vcount vertex_attr
#' @export
vrt_get_names.igraph <- function(x) {
  out <- vertex_attr(x, "name")
  if (is.null(out)) {
    out <- seq_len(vcount(x))
  }
  out
}

#' @rdname vrt_get_names
#' 
#' @export
vrt_get_names.network <- function(x) {
  out <- unlist(lapply(x[["val"]], `[[`, "vertex.names"))
  if (is.null(out)) {
    out <- seq_len(x[["gal"]][["n"]])
  }
  out
}

#' @rdname vrt_get_names
#' 
#' @export
vrt_get_names.tbl_graph <- function(x) {
  vrt_get_names.igraph(as_igraph.tbl_graph(x))
}

#* ====