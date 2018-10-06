#' Conversion to `igraph` objects.
#' 
#' Accurately map foreign graph data to `igraph` objects.
#' 
#' @details
#' `as_igraph()` converts `x` to an intermediate `bridge_net` object that is capable of 
#' mapping metadata, edges, vertices, and attributes (edge, vertex, and graph-level)
#' to a new, valid `igraph` object. \cr
#' 
#' @param x [`network::network`] (or [`tidygraph::tbl_graph`]) object.
#' 
#' @return An `igraph` ([`igraph::graph`]) object.
#' 
#' @seealso [as_network()], [intergraph::asIgraph()]
#' 
#' @template bknapp-author
#' 
#' @export
as_igraph <- function(x) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' 
#' @importFrom igraph add_edges edge_attr<- graph_attr<- make_empty_graph vertex_attr<- V<-
as_igraph.bridge_net <- function(x) {
  metadata <- x[["metadata"]]
  out <- make_empty_graph(n = metadata[["n_vertices"]], 
                          directed = metadata[["is_directed"]])
  out <- add_edges(out, edges = t(`names<-`(x[["edges"]][, c(".ego", ".alter")], NULL)))
  x[["edges"]][[".ego"]] <- NULL
  x[["edges"]][[".alter"]] <- NULL
  if (ncol(x[["edges"]])) {
    edge_attr(out) <- x[["edges"]]
  }
  if (".actor" %in% names(x[["vertices"]])) {
    x[["vertices"]][["type"]] <- x[["vertices"]][[".actor"]]
  }
  names(x[["vertices"]])[names(x[["vertices"]]) == ".vrt_name"] <- "name"
  vertex_attr(out) <- x[["vertices"]]
  out
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.igraph <- function(x) {
  x
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.network <- function(x) {
  as_igraph.bridge_net(as_bridge_net(x))
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.tbl_graph <- function(x) {
  to_keep <- "class"
  current <- names(attributes(x))
  for (i in seq_along(current)) {
    if (!current[[i]] %in% to_keep) `attr<-`(x, current[[i]], NULL)
  }
  class(x) <- "igraph"
  x
}


#' @rdname as_igraph
#' 
#' @importFrom igraph as.igraph
#' @export
as.igraph.network <- function(x) {
  as_igraph.network(x)
}

#' @rdname as_igraph
#' 
#' @export
igraph::as.igraph


