get_metadata <- function(x, full = FALSE) {
  UseMethod("get_metadata")
}

get_metadata.default <- function(x, full = FALSE) {
  out <- list(n_vertices = net_count_vertices(x),
              n_edges = net_count_edges(x),
              is_directed = net_is_directed(x),
              is_bipartite = net_is_bipartite(x),
              n_actors = NULL)
  if (net_is_bipartite(x)) {
    out[["n_actors"]] <- net_count_actors(x)
  }
  if (full) {
    out[["any_multiplex"]] <- net_is_multiplex(x)
    out[["has_loops"]] <- net_has_loops(x)
    out[["has_isolates"]] <- net_has_isolates(x)
  }
  out <- Filter(length, out)
  class(out) <- c("metadata", "list")
  out
}

get_metadata.edgelist <- function(x, full = FALSE) {
  out <- list(n_vertices = attr(x, "n_vertices"),
              n_edges = attr(x, "n_edges"),
              is_directed = attr(x, "is_directed"),
              is_bipartite = attr(x, "is_bipartite"),
              n_actors = NULL)
  if (net_is_bipartite(x)) {
    out[["n_actors"]] <- attr(x, "n_actors")
  }
  if (full) {
    out[["any_multiplex"]] <- net_is_multiplex(x)
    out[["has_loops"]] <- net_has_loops(x)
    out[["has_isolates"]] <- net_has_isolates(x)
  }
  out <- Filter(length, out)
  class(out) <- c("metadata", "list")
  out
}

set_metadata_attr <- function(x, graph) {
  metadata <- get_metadata(graph)
  for (i in seq_along(metadata)) {
    attr(x, names(metadata)[[i]]) <- metadata[[i]]
  }
  x
}

print.net_metadata <- function(x) {
  cat("# A `net_metadata` list.")
  cat("\n")
  print(`colnames<-`(cbind(x), ""))
}

#' Is a graph object bipartite?
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `logical` indicating whether `x` is bipartite.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- igraph::bipartite.random.game(7, 3, type = "gnp", p = 0.4)
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' el <- ig %>% 
#'   rep_as_edgelist(use_names = FALSE)
#' 
#' @export
net_is_bipartite <- function(x) {
  UseMethod("net_is_bipartite")
}

#' @rdname net_is_bipartite
#'
#' @examples 
#' net_is_bipartite(bridge_g)
#' 
#' @export
net_is_bipartite.bridge_net <- function(x) {
  x[["metadata"]][["is_bipartite"]]
}

#' @rdname net_is_bipartite
#' 
#' @seealso [igraph::is_bipartite()]
#' 
#' @examples 
#' net_is_bipartite(ig)
#' 
#' @export
net_is_bipartite.igraph <- function(x) {
  "type" %in% vrt_attr_names(x)
}

#' @rdname net_is_bipartite
#' 
#' @seealso [network::is.bipartite()]
#' 
#' @examples 
#' net_is_bipartite(nw)
#' 
#' @export
net_is_bipartite.network <- function(x) {
  is.numeric(x[["gal"]][["bipartite"]])
}

#' @rdname net_is_bipartite
#' 
#' @seealso [tidygraph::graph_is_bipartite()]
#' 
#' @examples 
#' net_is_bipartite(tidy_g)
#'
#' @export
net_is_bipartite.tbl_graph <- function(x) {
  net_is_bipartite(as_igraph(x))
}

#' @rdname net_is_bipartite
#' 
#' @examples 
#' net_is_bipartite(el)
#' 
#' @export
net_is_bipartite.edgelist <- function(x) {
  attr(x, "is_bipartite")
}

#' Count the number of vertices belonging to the actor mode of a bipartite graph object.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `integer` corresponding to the number `x`'s actors.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- igraph::bipartite.random.game(7, 3, type = "gnp", p = 0.4)
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' el <- ig %>% 
#'   rep_as_edgelist(use_names = FALSE)
#' 
#' @export
net_count_actors <- function(x) {
  UseMethod("net_count_actors")
}

#' @rdname net_count_actors
#' 
#' @examples 
#' net_count_actors(ig)
#' 
#' @export
net_count_actors.igraph <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` are only applicable to bipartite networks.")
  }
  set_whole_number_storage(length(which(vertex_attr(x, "type"))))
}

#' @rdname net_count_actors
#' 
#' @examples 
#' net_count_actors(nw)
#' 
#' @export
net_count_actors.network <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` only exist in bipartite networks.")
  }
  set_whole_number_storage(x[["gal"]][["bipartite"]])
}

#' @rdname net_count_actors
#' 
#' @examples 
#' net_count_actors(bridge_g)
#' 
#' @export
net_count_actors.bridge_net <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` only exist in bipartite networks.")
  }
  set_whole_number_storage(x[["metadata"]][["n_actors"]])
}

#' @rdname net_count_actors
#' 
#' @examples 
#' net_count_actors(tidy_g)
#' 
#' @export
net_count_actors.tbl_graph <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` only exist in bipartite networks.")
  }
  as.integer(net_count_actors(as_igraph(x)))
}

#' @rdname net_count_actors
#' 
#' @examples 
#' net_count_actors(el)
#' 
#' @export
net_count_actors.edgelist <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` only exist in bipartite networks.")
  }
  attr(x, "n_actors")
}


#' Are a network's edges directed or undirected?
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `logical` indicating whether `x`'s edges are directed.
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- igraph::random.graph.game(10, 0.25, directed = TRUE)
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' el <- ig %>% 
#'   rep_as_edgelist(use_names = FALSE)
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
net_is_directed <- function(x) {
  UseMethod("net_is_directed")
}

#' @rdname net_is_directed
#' 
#' @examples 
#' net_is_directed(bridge_g)
#'
#' @export
net_is_directed.bridge_net <- function(x) {
  x[["metadata"]][["is_directed"]]
}

#' @rdname net_is_directed
#' 
#' @seealso [igraph::is_directed()]
#' 
#' @examples 
#' net_is_directed(ig)
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
#' @examples 
#' net_is_directed(nw)
#' 
#' @export
net_is_directed.network <- function(x) {
  is_true(x[["gal"]][["directed"]])
}

#' @rdname net_is_directed
#' 
#' @seealso [tidygraph::graph_is_directed()]
#' 
#' @examples 
#' net_is_directed(tidy_g)
#' 
#' @export
net_is_directed.tbl_graph <- function(x) {
  net_is_directed(as_igraph(x))
}

#' @rdname net_is_directed
#' 
#' @examples 
#' net_is_directed(el)
#'
#' @export
net_is_directed.edgelist <- function(x) {
  attr(x, "is_directed")
}



#' Count the number of vertices present in a graph object.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `integer` indicating the number of vertices in `x`.
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- igraph::random.graph.game(10, 0.25, directed = TRUE)
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' el <- ig %>% 
#'   rep_as_edgelist(use_names = FALSE)
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
net_count_vertices <- function(x) {
  UseMethod("net_count_vertices")
}

#' @rdname net_count_vertices
#' 
#' @examples
#' net_count_vertices(bridge_g)
#'
#' @export
net_count_vertices.bridge_net <- function(x) {
  set_whole_number_storage(x[["metadata"]][["n_vertices"]])
}

#' @rdname net_count_vertices
#' 
#' @seealso [igraph::vcount()]
#' 
#' @examples
#' net_count_vertices(ig)
#' 
#' @importFrom igraph vcount
#' @export
net_count_vertices.igraph <- function(x) {
  set_whole_number_storage(vcount(x))
}

#' @rdname net_count_vertices
#' 
#' @seealso [network::network.size()]
#' 
#' @examples
#' net_count_vertices(nw)
#' 
#' @export
net_count_vertices.network <- function(x) {
  set_whole_number_storage(x[["gal"]][["n"]])
}

#' @rdname net_count_vertices
#' 
#' @examples
#' net_count_vertices(tidy_g)
#' 
#' @export
net_count_vertices.tbl_graph <- function(x) {
  net_count_vertices(as_igraph(x))
}

#' @rdname net_count_vertices
#' 
#' @examples 
#' net_count_vertices(el)
#' 
#' @export
net_count_vertices.edgelist <- function(x) {
  attr(x, "n_vertices")
}



#' Count the number of edges present in a graph object.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `integer` indicating the number of edges in `x`.
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- igraph::random.graph.game(10, 0.25, directed = TRUE)
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' el <- ig %>% 
#'   rep_as_edgelist(use_names = FALSE)
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
net_count_edges <- function(x) {
  UseMethod("net_count_edges")
}

#' @rdname net_count_edges
#' 
#' @examples 
#' net_count_edges(bridge_g)
#' 
#' @export
net_count_edges.bridge_net <- function(x) {
  set_whole_number_storage(x[["metadata"]][["n_edges"]])
}

#' @rdname net_count_edges
#' 
#' @seealso [igraph::ecount()]
#' 
#' @examples 
#' net_count_edges(ig)
#'
#' @importFrom igraph ecount
#' @export
net_count_edges.igraph <- function(x) {
  set_whole_number_storage(ecount(x))
}

#' @rdname net_count_edges
#' 
#' @seealso [network::network.edgecount()]
#' 
#' @examples 
#' net_count_edges(nw)
#' 
#' @importFrom network network.edgecount
#' @export
net_count_edges.network <- function(x) {
  network.edgecount(x)
}

#' @rdname net_count_edges
#' 
#' @examples 
#' net_count_edges(tidy_g)
#' 
#' @export
net_count_edges.tbl_graph <- function(x) {
  net_count_edges(as_igraph(x))
}

#' @rdname net_count_edges
#' 
#' @examples 
#' net_count_edges(el)
#' 
#' @export
net_count_edges.edgelist <- function(x) {
  attr(x, "n_edges")
}

#' Is a graph object multiplex?
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `logical` indicating whether `x` is multiplex (It contains parallel edges).
#' 
#' @details `net_is_multiplex()` determines that edges are parallel as follows: \cr
#'  * If `x` is directed, edges are parallel if they share the same source vertex (`.ego`) 
#'   and same target vertex (`.alter`)
#'  * If `x` is undirected, edges are parallel if they share the same dyad, regardless of
#'   which vertex is stored as `.ego` or `.alter`.
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- c(1, 2,
#'         2, 3,
#'         1, 3,
#'         4, 1,
#'         1, 4) %>% 
#'  matrix(ncol = 2, byrow = TRUE) %>% 
#'  igraph::graph_from_edgelist(directed = FALSE)
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
net_is_multiplex <- function(x) {
  UseMethod("net_is_multiplex")
}

#' @rdname net_is_multiplex
#' 
#' @examples 
#' net_is_multiplex(bridge_g)
#' 
#' @export
net_is_multiplex.bridge_net <- function(x) {
  x[["metadata"]][["any_multiplex"]]
}

#' @rdname net_is_multiplex
#' 
#' @seealso [igraph::any_multiple()]
#' 
#' @examples 
#' net_is_multiplex(ig)
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
#' @examples 
#' net_is_multiplex(nw)
#' 
#' @export
net_is_multiplex.network <- function(x) {
  any(duplicated.matrix(get_el.network(x, vrt_attr = NULL)))
}

#' @rdname net_is_multiplex
#' 
#' @examples 
#' net_is_multiplex(tidy_g)
#' 
#' @export
net_is_multiplex.tbl_graph <- function(x) {
  net_is_multiplex(as_igraph(x))
}

#' @rdname net_is_multiplex
#' 
#' @examples 
#' ig %>% 
#'   rep_as_edgelist(use_names = FALSE) %>% 
#'   net_is_multiplex()
#' 
#' ig %>% 
#'   rep_as_edgelist(use_names = FALSE, weights = TRUE) %>% 
#'   net_is_multiplex()
#' 
#' @export
net_is_multiplex.edgelist <- function(x) {
  if (is.matrix(x)) {
    return(any(duplicated.matrix(x)))
  }
  any(duplicated.matrix(as.matrix(x[, 1:2])))
}


#' Does a graph object contain loops?
#' 
#' Test whether the `.ego` (source) and `.alter` (target) of any edge is the same vertex.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `logical` indicating whether `x` contains loop edges.
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- c(1, 2,
#'         2, 3,
#'         1, 3,
#'         4, 1,
#'         1, 4,
#'         2, 2) %>% 
#'  matrix(ncol = 2, byrow = TRUE) %>% 
#'  igraph::graph_from_edgelist(directed = FALSE)
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' el <- ig %>% 
#'   rep_as_edgelist(use_names = FALSE)
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
net_has_loops <- function(x) {
  UseMethod("net_has_loops")
}

#' @rdname net_has_loops
#' 
#' @examples 
#' net_has_loops(bridge_g)
#' 
#' @export
net_has_loops.bridge_net <- function(x) {
  x[["metadata"]][["has_loops"]]
}

#' @rdname net_has_loops
#' 
#' @seealso [igraph::which_loop()]
#' 
#' @examples 
#' net_has_loops(ig)
#' 
#' @importFrom igraph which_loop
#' @export 
net_has_loops.igraph <- function(x) {
  any(which_loop(x))
}

#' @rdname net_has_loops
#' 
#' @details 
#' `net_has_loops()` differs from `network::has.loops()` in that it checks whether loops 
#' actually exist, rather than whether loops are allowed.
#' 
#' @seealso [network::has.loops()]
#' 
#' @examples 
#' net_has_loops(nw)
#' 
#' @export 
net_has_loops.network <- function(x) {
  el <- rep_as_edgelist(x, use_names = FALSE, leave_raw = TRUE)
  any(el[, ".ego"] == el[, ".alter"])
}

#' @rdname net_has_loops
#' 
#' @examples 
#' net_has_loops(tidy_g)
#' 
#' @seealso [tidygraph::edge_is_loop()]
#' 
#' @export
net_has_loops.tbl_graph <- function(x) {
  net_has_loops.igraph(as_igraph.tbl_graph(x))
}

#' @rdname net_has_loops
#' 
#' @examples 
#' net_has_loops(el)
#' 
#' @export
net_has_loops.edgelist <- function(x) {
  any(x[, ".ego"] == x[, ".alter"])
}

#' Does a graph object contain isolates?
#' 
#' Test whether any vertices have 0 edges.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' 
#' @return `logical` indicating whether `x` contains isolated vertices.
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- igraph::make_empty_graph(n = 5) %>% 
#'   igraph::add_edges(c(1, 2, 2, 3, 4, 1, 2, 4))
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' bridge_g <- ig %>% 
#'   as_bridge_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' el <- ig %>% 
#'   rep_as_edgelist(use_names = FALSE)
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
net_has_isolates <- function(x) {
  UseMethod("net_has_isolates")
}

#' @rdname net_has_isolates
#' 
#' @examples 
#' net_has_isolates(bridge_g)
#' 
#' @export
net_has_isolates.bridge_net <- function(x) {
  n_with_edges <- length(unique(c(x[["edges"]][, c(".ego", ".alter")])))
  x[["metadata"]][["n_vertices"]] > n_with_edges
}

#' @rdname net_has_isolates
#' 
#' @examples 
#' net_has_isolates(ig)
#' 
#' @importFrom igraph degree
#' @export 
net_has_isolates.igraph <- function(x) {
  any(degree(x, mode = "total") == 0L)
}

#' @rdname net_has_isolates
#' 
#' @examples 
#' net_has_isolates(nw)
#' 
#' @export 
net_has_isolates.network <- function(x) {
  n_with_edges <- length(unique(c(rep_as_edgelist(x, use_names = FALSE, 
                                                  leave_raw = TRUE))))
  x[["gal"]][["n"]] > n_with_edges
}

#' @rdname net_has_isolates
#' 
#' @examples 
#' net_has_isolates(tidy_g)
#' 
#' @seealso [tidygraph::node_is_isolated()]
#' 
#' @export 
net_has_isolates.tbl_graph <- function(x) {
  net_has_isolates.igraph(as_igraph.tbl_graph(x))
}

#' @rdname net_has_isolates
#' 
#' @examples 
#' net_has_isolates(el)
#' 
#' @export 
net_has_isolates.edgelist <- function(x) {
  attr(x, "n_vertices") > length(unique(c(x)))
}

