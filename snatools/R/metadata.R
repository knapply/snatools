#' @title Is a Graph Directed?
#' 
#' @template graph-param
#' 
#' @return `logical` indicating whether `x` is a directed graph.
#' 
#' @template bknapp-author
#' 
#' @export
net_is_directed <- function(x) {
  UseMethod("net_is_directed")
}

#' @rdname net_is_directed
#'
#' @seealso [igraph::is_directed()]
#'
#' @importFrom igraph is_directed 
#' @export
net_is_directed.igraph <- function(x) {
  is_directed(x)
}

#' @rdname net_is_directed
#' 
#' @seealso [network::is.directed()]
#'
#' @export
net_is_directed.network <- function(x) {
  identical(x[["gal"]][["directed"]], TRUE)
}

#' @rdname net_is_directed
#' 
#' @seealso [tidygraph::graph_is_directed()]
#'
#' @export
net_is_directed.tbl_graph <- function(x) {
  net_is_directed.igraph(as_igraph.tbl_graph(x))
}

net_is_directed.bridge_net <- function(x) {
  x[["metadata"]][["is_directed"]]
}

#* ====

#' @title Is a Graph Bipartite?
#' 
#' @template graph-param
#' 
#' @return `logical` indicating whether `x` is a bipartite graph.
#' 
#' @template bknapp-author
#' 
#' @export
net_is_bipartite <- function(x) {
  UseMethod("net_is_bipartite")
}

#' @rdname net_is_bipartite
#'
#' @seealso [igraph::is_bipartite()]
#'  
#' @importFrom igraph vertex_attr_names
#' @export
net_is_bipartite.igraph <- function(x) {
  "type" %in% vertex_attr_names(x)
}

#' @rdname net_is_bipartite
#' 
#' @seealso [network::is.bipartite()]
#' 
#' @export
net_is_bipartite.network <- function(x) {
  is.numeric(x[["gal"]][["bipartite"]])
}

#' @rdname net_is_bipartite
#' 
#' @seealso [tidygraph::graph_is_bipartite()]
#'
#' @export
net_is_bipartite.tbl_graph <- function(x) {
  net_is_bipartite.igraph(as_igraph.tbl_graph(x))
}

net_is_bipartite.bridge_net <- function(x) {
  x[["metadata"]][["is_bipartite"]]
}

#* ====

#' @title How Many Vertices Belong to a Bipartite Graph's Actor Mode?
#' 
#' @template graph-param
#' 
#' @return `integer` indicating the number of `x`'s actor vertices.
#' 
#' @template bknapp-author
#' 
#' @export
net_count_actors <- function(x) {
  UseMethod("net_count_actors")
}

#' @rdname net_count_actors
#' 
#' @export
net_count_actors.igraph <- function(x) {
  if (!net_is_bipartite.igraph(x)) {
    glue_stop("`actors` are only applicable to bipartite networks.")
  }
  `storage.mode<-`(length(which(vertex_attr(x, "type"))), "integer")
}

#' @rdname net_count_actors
#' 
#' @export
net_count_actors.network <- function(x) {
  if (!net_is_bipartite.network(x)) {
    glue_stop("`actors` only exist in bipartite networks.")
  }
  `storage.mode<-`(x[["gal"]][["bipartite"]], "integer")
}

#' @rdname net_count_actors
#' 
#' @export
net_count_actors.tbl_graph <- function(x) {
  if (!net_is_bipartite(x)) {
    glue_stop("`actors` only exist in bipartite networks.")
  }
  net_count_actors.igraph(as_igraph.tbl_graph(x))
}

net_count_actors.bridge_net <- function(x) {
  if (x[["metadata"]][["bipartite"]]) {
    return(x[["metadata"]][["n_actors"]])
  }
  NULL
}

#* ====

#' @title How Many Vertices Does a Graph Have?
#' 
#' @template graph-param
#' 
#' @return `integer` indicating the total number of `x`'s vertices.
#' 
#' @template bknapp-author
#' 
#' @export
net_count_vertices <- function(x) {
  UseMethod("net_count_vertices")
}

#' @rdname net_count_vertices
#' 
#' @seealso [igraph::vcount()]
#' 
#' @importFrom igraph vcount
#' @export
net_count_vertices.igraph <- function(x) {
  `storage.mode<-`(vcount(x), "integer")
}

#' @rdname net_count_vertices
#' 
#' @seealso [network::network.size()]
#' 
#' @export
net_count_vertices.network <- function(x) {
  `storage.mode<-`(x[["gal"]][["n"]], "integer")
}

#' @rdname net_count_vertices
#' 
#' @seealso [tidygraph::graph_order()]
#' 
#' @export
net_count_vertices.tbl_graph <- function(x) {
  net_count_vertices.igraph(as_igraph.tbl_graph(x))
}

net_count_vertices.bridge_net <- function(x) {
  nrow(x[["vertices"]])
}

#* ====

#' @title How Many Edges Does a Graph Have?
#' 
#' @template graph-param
#' 
#' @return `integer` indicating the total number of `x`'s edges.
#' 
#' @template bknapp-author
#' 
#' @export
net_count_edges <- function(x) {
  UseMethod("net_count_edges")
}

#' @rdname net_count_edges
#' 
#' @seealso [igraph::ecount()]
#'
#' @importFrom igraph ecount
#' @export
net_count_edges.igraph <- function(x) {
  `storage.mode<-`(ecount(x), "integer")
}

#' @rdname net_count_edges
#' 
#' @seealso [network::network.edgecount()]
#' 
#' @importFrom network network.edgecount
#' @export
net_count_edges.network <- function(x) {
  `storage.mode<-`(network.edgecount(x), "integer")
}

#' @rdname net_count_edges
#' 
#' @seealso [tidygraph::graph_size()]
#' 
#' @export
net_count_edges.tbl_graph <- function(x) {
  net_count_edges.igraph(as_igraph.tbl_graph(x))
}

net_count_edges.bridge_net <- function(x) {
  nrow(x[["edges"]])
}

#* ====

#' @title Is a Graph Multiplex?
#' 
#' @template graph-param
#' 
#' @return `logical` indicating whether `x` is contains parallel edges.
#' 
#' @template bknapp-author
#' 
#' @export
net_is_multiplex <- function(x) {
  UseMethod("net_is_multiplex")
}

#' @rdname net_is_multiplex
#' 
#' @seealso [igraph::any_multiple()]
#' 
#' @importFrom igraph any_multiple
#' @export
net_is_multiplex.igraph <- function(x) {
  any_multiple(x)
}

#' @rdname net_is_multiplex
#' 
#' @seealso [network::is.multiplex()]
#' 
#' @export
net_is_multiplex.network <- function(x) {
  el <- get_el(x)
  if (!net_is_directed.network(x)) {
    el <- sort_el_cols_by_row(el)
  }
  any(duplicated.matrix(el))
}

#' @rdname net_is_multiplex
#' 
#' @seealso [tidygraph::graph_is_simple()]
#' 
#' @export
net_is_multiplex.tbl_graph <- function(x) {
  net_is_multiplex.igraph(as_igraph.tbl_graph(x))
}

net_is_multiplex.bridge_net <- function(x) {
  el <- cbind(x[["edges"]][[".ego"]],
              x[["edges"]][[".alter"]])
  if (x[["metadata"]][["directed"]]) {
    return(
      nrow(unique.matrix(sort_el_cols_by_row(el))) < nrow(el)
    )
  }
  nrow(unique.matrix(el)) < nrow(el)
}

#* ====

#' @title Does a Graph Have Loops?
#' 
#' @template graph-param
#' 
#' @return `logical` indicating whether `x` is contains loop edges.
#' 
#' @template bknapp-author
#' 
#' @export
net_has_loops <- function(x) {
  UseMethod("net_has_loops")
}

#' @rdname net_has_loops
#' 
#' @seealso [igraph::which_loop()]
#' 
#' @importFrom igraph which_loop
#' @export 
net_has_loops.igraph <- function(x) {
  any(which_loop(x))
}

#' @rdname net_has_loops
#' 
#' @seealso [network::has.loops()]
#' 
#' @export 
net_has_loops.network <- function(x) {
  el <- get_el.network(x)
  any(el[, 1L] == el[, 2L])
}

#' @rdname net_has_loops
#' 
#' @seealso [tidygraph::edge_is_loop()]
#' 
#' @export
net_has_loops.tbl_graph <- function(x) {
  net_has_loops.igraph(as_igraph.tbl_graph(x))
}

net_has_loops.bridge_net <- function(x) {
  el <- cbind(x[["edges"]][[".ego"]],
              x[["edges"]][[".alter"]])
  any(el[, 1L] == el[, 2L])
}

#* ====

#' @title Does a Graph Have Isolates?
#' 
#' @template graph-param
#' 
#' @return `logical` indicating whether `x` is contains isolated vertices.
#' 
#' @template bknapp-author
#' 
#' @export
net_has_isolates <- function(x) {
  UseMethod("net_has_isolates")
}

#' @rdname net_has_isolates
#' 
#' @importFrom igraph degree
#' @export 
net_has_isolates.igraph <- function(x) {
  any(degree(x, mode = "total") == 0)
}

#' @rdname net_has_isolates
#' 
#' @export 
net_has_isolates.network <- function(x) {
  x[["gal"]][["n"]] > length(unique(`dim<-`(get_el.network(x), NULL)))
}

#' @rdname net_has_isolates
#' 
#' @seealso [tidygraph::node_is_isolated()]
#' 
#' @export 
net_has_isolates.tbl_graph <- function(x) {
  net_has_isolates.igraph(as_igraph.tbl_graph(x))
}

net_has_isolates.bridge_net <- function(x) {
  any(!x[["vertices"]][[".vrt_id"]] %in%
        unique(c(x[["edges"]][[".ego"]],
                 x[["edges"]][[".alter"]])))
}

#* ====

get_metadata <- function(x) {
  out <- list(n_vertices = net_count_vertices(x),
              n_edges = net_count_edges(x),
              is_directed = net_is_directed(x),
              is_bipartite = net_is_bipartite(x),
              n_actors = NULL)
  if (net_is_bipartite(x)) {
    out[["n_actors"]] <- net_count_actors(x)
  }
  drop_nulls(out)
}
# 
# set_metadata_attr <- function(x, graph) {
#   metadata <- get_metadata(graph)
#   for (i in seq_along(metadata)) {
#     attr(x, names(metadata)[[i]]) <- metadata[[i]]
#   }
#   x
# }

