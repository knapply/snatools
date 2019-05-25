get_test_mat <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
                         .diag = FALSE, .storage = c("int", "dbl")) {
  .storage <- switch(
    match.arg(.storage, c("int", "dbl"), several.ok = FALSE),
    int = "integer", 
    dbl = "double"
  )
  
  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
    
    out <- structure(
      c(1L, 1L, 0L, 1L, 0L, 1L,
        1L, 0L, 2L, 0L, 0L, 1L, 
        0L, 1L, 1L, 1L, 0L, 0L, 
        1L, 0L, 1L, 0L, 2L, 0L, 
        0L, 0L, 0L, 1L, 1L, 1L, 
        0L, 2L, 0L, 0L, 1L, 1L),
      .Dim = c(6L, 6L)
    )

    # return(out)

  } else {
    out <- structure(
      c(0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L,
        1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 1L, 0L,
        0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L,
        0L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L,
        1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L,
        0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 2L,
        0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 1L, 0L,
        0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L,
        0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
      .Dim = c(12L, 12L)
    )

  }
  
  if (!.diag) {
    diag(out) <- 0L
  }
  
  if (!.weighted) {
    out[out == 2L] <- 1L
  }
  
  if (!.directed) {
    out[] <- 0.5 * (out + t(out))
    # out[lower.tri(out)] <- 0L
  }

  if (typeof(out) != .storage) {
    storage.mode(out) <- .storage
  }
  
  out
}

el_from_adj_mat <- function(.adj_mat, .weighted = FALSE, ...) {
  n_cols <- ifelse(.weighted, 3L, 2L)
  
  melted <- matrix(c(row(.adj_mat), col(.adj_mat), as.vector(.adj_mat)), ncol = 3L)
  
  melted[melted[, 3L] != 0, seq_len(n_cols)]
}

get_test_el <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
                        .diag = FALSE, .storage = c("int", "dbl"), ...) {
  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
  }
  
  adj_mat <- get_test_mat(.directed = .directed, .bipartite = .bipartite,
                          .weighted = .weighted, .diag = .diag)
  out <- el_from_adj_mat(adj_mat, .weighted = .weighted, ...)
  
  # out[ , 1L:2L] <- cbind(
  #   pmin.int(out[, 1L], out[, 2L]), pmax.int(out[, 1L], out[, 2L])
  # )
  
  out
}




get_test_ig <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, 
                        .diag = FALSE, ...) {
  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
  }
  
  el <- get_test_el(.directed = .directed, .bipartite = .bipartite, 
                    .weighted = .weighted, .diag = .diag)
  n_nodes <- length(unique(as.vector(el[ , 1L:2L])))
  
  out <- igraph::make_empty_graph(n = n_nodes, directed = .directed)
  
  if (.weighted) {
    out <- igraph::add_edges(out, edges = t(el[ , 1L:2L]), 
                             attr = list(weight = el[ , 3L]))
  } else {
    out <- igraph::add_edges(out, edges = t(el[ , 1L:2L]))
  }
  
  if (.bipartite) {
    out <- igraph::set_vertex_attr(
      out, name = "type", value = c(rep(TRUE, n_nodes %/% 2), rep(FALSE, n_nodes %/%  2))
    )
  }
  out
}

get_test_nw <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, 
                        .diag = FALSE, ...) {
  el <- get_test_el(.directed = .directed, .bipartite = .bipartite, 
                    .weighted = .weighted, .diag = .diag)
  n_nodes <- length(unique(as.vector(el[ , 1L:2L])))

  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
    .bipartite <- n_nodes %/% 2
  } else {
    .bipartite <- NULL
  }

  out <- network::network.initialize(
    n = n_nodes,  directed = .directed, loops = .diag,
    hyper = FALSE, multiple = FALSE, bipartite = .bipartite
  )

  out <- network::add.edges.network(out,  head = el[ , 1L], tail = el[ , 2L])

  if (.weighted) {
    out <- network::set.edge.attribute(out, attrname = "weight", value = el[ , 3L])
  }

  out
}




.sort_edge_indices <- function(.x) {
  cbind(
    pmin(.x[, 1L], .x[, 2L]), pmax(.x[, 1L], .x[, 2L])
  )
}


build_test_args <- function() {
  init <- list(
    list(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, .diag = FALSE), 
    list(.directed = FALSE, .bipartite = TRUE, .weighted = TRUE, .diag = TRUE)
  )

  out <- expand.grid(
    do.call(rbind.data.frame, init)
  )

  apply(out, 1L, as.list)
}

# .rep_as_edgelist <- function(.x) {
#   UseMethod(".rep_as_edgelist")
# }
# .rep_as_edgelist.igraph <- function(.x) {
#   igraph::as_edgelist(.x)
# }
# 
# .rep_as_edgelist.network <- function(.x) {
#   network::as_edgelist(.x)
# }

el <- do.call(get_test_ig, test_args[[1]]) %>% igraph::as_edgelist()

.sort_el <- function(.el) {
  .el[order(.el[ , 1L], .el[ , 2L ]) , ]
}


test_all <- function() {
  test_args <- build_test_args()
  # failures <- list()
  
  for (i in seq_along(test_args)) {
    res <- .all_equal(
      .sort_el(.fetch_edgelist(do.call(get_test_ig, test_args[[i]]))),
      .sort_el(.fetch_edgelist(do.call(get_test_nw, test_args[[i]])))
    )
    if (!res) {
      return(test_args[[i]])
    }
  }
  TRUE
}


