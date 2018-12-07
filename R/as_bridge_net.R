as_bridge_net <- function(x) {
  if (inherits(x, "igraph") && net_is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
    n_actors <- net_count_actors(x)
  } else {
    n_actors <- NULL
  }
  out <- list(
    metadata = list(n_vertices = net_count_vertices(x),
                    n_edges = net_count_edges(x),
                    is_directed = net_is_directed(x),
                    is_bipartite = net_is_bipartite(x),
                    n_actors = n_actors,
                    is_multiplex = net_is_multiplex(x),
                    has_loops = net_has_loops(x),
                    has_isolates = net_has_isolates(x)),
    edges = as.data.frame(x, .unit = "edges"),
    vertices = as.data.frame(x, .unit = "vertices")
    )
  class(out) <- "bridge_net"
  out
}