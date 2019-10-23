adj_mat_as_network <- function(adj_mat, directed = TRUE, allow_loops = FALSE, ...) {
  edge_list <- adj_mat_as_el(adj_mat, directed = directed, allow_loops = allow_loops)

  out <- network::network.initialize(
    n = nrow(adj_mat),
    directed = directed,
    hyper = FALSE,
    loops = allow_loops,
    multiple = FALSE,
    bipartite = FALSE
  )

  network::add.edges.network(
    x = out,
    tail = edge_list[, 1L],
    head = edge_list[, 2L]
  )

  if (ncol(edge_list) == 3L) {
    network::set.edge.attribute(out, attrname = "weight", value = edge_list[, 3L])
  }

  out
}


el_as_network <- function(edge_list, directed = TRUE, allow_loops = FALSE,
                          allow_weights = FALSE,
                          allow_parallel_edges = FALSE, ...) {
  .throw_invalid_el(
    edge_list = edge_list,
    directed = directed,
    allow_loops = allow_loops,
    allow_weights = allow_weights,
    allow_parallel_edges = allow_parallel_edges
  )

  n_nodes <- max(edge_list[, c(1L, 2L)])

  out <- network::network.initialize(
    n = max(edge_list[, c(1L, 2L)]),
    directed = directed,
    hyper = FALSE,
    loops = allow_loops,
    multiple = allow_parallel_edges,
    bipartite = FALSE
  )

  network::add.edges.network(
    x = out,
    tail = edge_list[, 1L],
    head = edge_list[, 2L],
    edge.check = FALSE
  )

  if (allow_weights && el_is_weighted(edge_list)) {
    network::set.edge.attribute(out, attrname = "weight", value = edge_list[, 3L])
  }

  out
}



# adj_mat <- example_adj_mat(directed = F)
# adj_mat_as_network(example_adj_mat(loops = TRUE),
                   # loops = TRUE)
# all(.rep_as_el(example_igraph(directed = TRUE)) == .rep_as_el(adj_mat_as_network(example_adj_mat(directed = TRUE))))
