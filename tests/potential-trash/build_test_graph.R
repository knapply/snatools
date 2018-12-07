#' @importFrom stats runif
build_test_graph <- function(ig_or_nw, n_nodes = 10L,
                             directed = TRUE, bipartite = FALSE,
                             isolate = FALSE, seed = 1234) {
  n_edges <- NULL
  if (!bipartite) {
    potential_ties <- n_nodes^2
    set.seed(seed)
    n_edges <- sample(seq(0, potential_ties), 1L)
    if (n_edges) {
      ties <- rep(1L, n_edges)
      non_ties <- rep(0L, potential_ties - length(ties))
      edge_vec <- c(ties, non_ties)
      set.seed(seed)
      mat1 <- matrix(sample(edge_vec), nrow = n_nodes, ncol = n_nodes)
      set.seed(seed)
      mat2 <- matrix(sample(edge_vec), nrow = n_nodes, ncol = n_nodes)
    } else {
      mat1 <- matrix(0L, nrow = n_nodes, ncol = n_nodes)
      mat2 <- mat1
    }
    if (!directed) {
      mat1[upper.tri(mat1)] <- 0L
      mat2[upper.tri(mat2)] <- 0L
    }
  }
  if (bipartite && n_nodes > 1L) {
    set.seed(seed)
    if (n_nodes == 0L) {
      n_actors <- 0L
    } else {
      n_actors <- sample(seq_len(n_nodes), 1L)
    }
    n_non_actors <- n_nodes - n_actors
    if (n_actors > 0L && n_non_actors > 0L) {
      mat1 <- matrix(nrow = n_actors, ncol = n_non_actors)
      set.seed(seed)
      n_edges <- sample(seq_len(length(mat1)), 1L)
      ties <- rep(1L, n_edges)
      non_ties <- rep(0L, length(mat1) - n_edges)
      edge_vec <- c(ties, non_ties)
      set.seed(seed)
      mat1 <- matrix(sample(edge_vec), nrow = nrow(mat1), ncol = ncol(mat1))
      set.seed(seed)
      mat2 <- matrix(sample(edge_vec), nrow = nrow(mat1), ncol = ncol(mat1))
    }
  }
  if (bipartite && n_nodes == 1L) {
    set.seed(seed)
    n_actors <- sample(0:1, size = 1L)
    mat1 <- matrix(0L, nrow = 1L, ncol = 1L)
    mat2 <- mat1
  }
  if (n_nodes == 0L) {
    mat1 <- matrix(integer(0))
    mat2 <- mat1
  }
  n_edges <- sum(c(mat1, mat2))

  if (n_nodes > 0L) {
    set.seed(seed)
    vertices_df <- data.frame(vertex.names = paste0("node_", seq_len(n_nodes)),
                              node_chr = sample(letters, n_nodes, replace = TRUE),
                              node_int = sample(seq_len(n_nodes), n_nodes),
                              node_dbl = runif(n_nodes, 0, 100),
                              node_lgl = sample(c(TRUE, FALSE), n_nodes, replace = TRUE),
                              stringsAsFactors = FALSE)
    # if (add_isolate) {
    #   vertices_df <- rbind.data.frame(data.frame(vertex.names = "the_isolate",
    #                                              node_chr = "isolate_chr",
    #                                              node_int = 0L,
    #                                              node_dbl = 2.2,
    #                                              node_lgl = FALSE,
    #                                              stringsAsFactors = FALSE),
    #                                   stringsAsFactors = FALSE)
    # }
  } else {
    vertices_df <- NULL
  }
  if (!is.null(vertices_df)) {
    if (bipartite) {
      types <- c(rep(TRUE, n_actors), rep(FALSE, n_nodes - n_actors))
      vertices_df[["type"]] <- types
      rownames(mat1) <- vertices_df[vertices_df$type, "vertex.names"]
      colnames(mat1) <- vertices_df[!vertices_df$type, "vertex.names"]
      rownames(mat2) <- rownames(mat1)
      colnames(mat2) <- colnames(mat1)
    }
    if (!bipartite) {
      rownames(mat1) <- vertices_df[["vertex.names"]]
      colnames(mat1) <- vertices_df[["vertex.names"]]
      rownames(mat2) <- vertices_df[["vertex.names"]]
      colnames(mat2) <- vertices_df[["vertex.names"]]
    }
  }
  adj_mat_to_el <- function(mat) {
    egos <- c()
    alters <- c()
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        if (mat[i, j] == 1L) {
          egos <- c(egos, rownames(mat)[[i]])
          alters <- c(alters, colnames(mat)[[j]])
        }
      }
    }
    cbind(egos, alters)
  }
  if (!is.null(n_edges) && n_edges) {
    el <- rbind(adj_mat_to_el(mat1), adj_mat_to_el(mat2))
    n_edges <- nrow(el)
    set.seed(seed)
    edges_df <- data.frame(from = el[, 1],
                           to = el[, 2],
                           edge_chr = sample(letters, n_edges, replace = TRUE),
                           edge_int = sample(seq_len(n_edges), n_edges, replace = TRUE),
                           edge_dbl = runif(n_edges, 0, 1000),
                           edge_lgl = sample(c(TRUE, FALSE), n_edges, replace = TRUE),
                           stringsAsFactors = FALSE)

  } else {
    edges_df <- NULL
  }
  if (ig_or_nw == "ig") {
    out <- build_test_igraph(vattrs = vertices_df, eattrs = edges_df,
                             directed = directed, bipartite = bipartite)
    if (isolate) {
      out <- igraph::delete_vertices(out, 3L)
    }
  }
  if (ig_or_nw == "nw") {
    out <- build_test_network(vattrs = vertices_df, eattrs = edges_df,
                              directed = directed, bipartite = bipartite)
    if (isolate) {
      network::delete.vertices(out, 3L)
    }
  }
  out
}


build_test_igraph <- function(vattrs, eattrs, directed, bipartite) {
  graph_attrs <- list(graph_chr = "Much Graph. Many Attributes",
                    graph_lgl = TRUE,
                    graph_int = 1L,
                    graph_dbl = 3.14)
  if (!is.null(vattrs)) {
    n_nodes <- nrow(vattrs)
  } else {
    n_nodes <- 0L
  }
  out <- igraph::make_empty_graph(n = n_nodes, directed = directed)
  igraph::graph_attr(out) <- graph_attrs
  if (!is.null(vattrs)) {
    vattrs[["name"]] <- vattrs[["vertex.names"]]
    vattrs[["vertex.names"]] <- NULL
    igraph::vertex_attr(out) <- vattrs
  }
  if (is.null(vattrs) && bipartite) {
    igraph::V(out)$type <- NA
  }
  if (!is.null(eattrs)) {
    edges <- eattrs[, c("from", "to")]
    eattrs[["from"]] <- NULL
    eattrs[["to"]] <- NULL
    names(edges) <- NULL
    edges <- t(edges)
    out <- igraph::add_edges(out, edges)
    igraph::edge_attr(out) <- eattrs
  }
  out
}


build_test_network <- function(vattrs, eattrs, directed, bipartite) {
  graph_attrs <- list(graph_chr = "Much Graph. Many Attributes",
                      graph_lgl = TRUE,
                      graph_int = 1L,
                      graph_dbl = 3.14)
  if (!is.null(vattrs)) {
    n_nodes <- nrow(vattrs)
  } else {
    n_nodes <- 0L
  }
  if (bipartite) {
    if (!is.null(vattrs)) {
      bip_arg <- length(which(vattrs[, "type"]))
      vattrs[["type"]] <- NULL
    } else {
      bip_arg <- 0L
    }
  } else {
    bip_arg <- FALSE
  }
  out <- network::network.initialize(n_nodes, directed, hyper = FALSE,
                                     loops = TRUE, multiple = TRUE, bipartite = bip_arg)
  if (directed) {
    network::set.network.attribute(out, attrname = "directed", value = TRUE)
    out[["gal"]][["directed"]] <- TRUE
  }
  for (g in names(graph_attrs)) {
    network::set.network.attribute(out, g, graph_attrs[[g]])
  }
  if (!is.null(eattrs)) {
    egos <- match(eattrs[["from"]], vattrs[["vertex.names"]])
    alters <- match(eattrs[["to"]], vattrs[["vertex.names"]])
    if (directed) {
      el <- cbind(egos, alters)
    } else {
      el <- cbind(alters, egos)
    }
    network::add.edges(out, tail = el[, 1], head = el[, 2])
    eattrs[["from"]] <- NULL
    eattrs[["to"]] <- NULL
    for (e in colnames(eattrs)) {
      network::set.edge.attribute(out, e, value = eattrs[[e]])
    }
  }
  if (!is.null(vattrs)) {
    for (v in colnames(vattrs)) {
      network::set.vertex.attribute(out, v, value = vattrs[[v]])
    }
  }
  out
}
