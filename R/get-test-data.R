get_test_mat <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
                         .diag = FALSE, .storage = c("int", "dbl")) {
  .storage <- switch(
    match.arg(.storage, c("int", "dbl"), several.ok = FALSE),
    int = "integer", 
    dbl = "double"
  )
  
  if (.bipartite) {
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
  
  if (!.bipartite && !.diag) {
    diag(out) <- 0L
  }
  
  if (!.weighted) {
    out[out == 2L] <- 1L
  }
  
  if (!.directed) {
    out[] <- 0.5 * (out + t(out))
  }

  if (typeof(out) != .storage) {
    storage.mode(out) <- .storage
  }
  
  out
}

#' @importFrom igraph graph_from_adjacency_matrix graph_from_incidence_matrix
get_test_ig <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, 
                        .diag = FALSE, ...) {
  mat <- get_test_mat(.directed = .directed, .bipartite = .bipartite,
                      .weighted = .weighted, .diag = .diag, ...)
  .weighted <- .weighted %{F}% NULL

  if (.bipartite) {
    graph_from_incidence_matrix(mat, directed = .directed, weighted = .weighted)
  } else {
    graph_from_adjacency_matrix(mat, weighted = .weighted, diag = .diag)
  }
}

get_test_nw <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
                        .diag = FALSE, ...) {
  mat <- get_test_mat(.directed = .directed, .bipartite = .bipartite,
                      .weighted = .weighted, .diag = .diag, ...)
  
  bipartite_arg <- ifelse(.bipartite, ncol(mat), NULL)
  loops_arg <- ifelse(.diag, TRUE, FALSE)
  
  out <- network::as.network.matrix(mat, directed = .directed, weighted = .weighted,
                             bipartite = bipartite_arg, loops = loops_arg, 
                             names.eval = "weight")
  
  # if (.weighted) {
    # edge_weights <- 
  # }
  
  # if (.bipartite) {
  #   network::as.network.matrix(mat, directed = .directed, weighted = .weighted,
  #                              bipartite = ncol(mat), loops = FALSE, 
  #                              names.eval = "weight")
  # } else {
  #   network::as.network.matrix(mat, directed = .directed, loops = .diag)
  # }
}

# big_mat <- igraph::random.graph.game(n = 1000, p.or.m = 0.15) %>% 
#   igraph::as_adjacency_matrix()
# 
# 
# el_from_adj_mat <- function(.adj_mat, .weighted = FALSE, .col_major = TRUE, ...) {
#   if (!.weighted) {
#     n_cols <- 2L
#   } else {
#     if (any(!.adj_mat %in% 0:1)) {
#       n_cols <- 2L
#     } else {
#       n_cols <- 3L
#     }
#   }
#   if (.col_major) {
#     melted <- matrix(c(col(.adj_mat), row(.adj_mat), as.vector(.adj_mat)), ncol = 3L)
#     
#   } else {
#     melted <- matrix(c(row(.adj_mat), col(.adj_mat), as.vector(.adj_mat)), ncol = 3L)
#   }
#   
#   melted[melted[, 3L] != 0, seq_len(n_cols)]
# }
# 
# microbenchmark::microbenchmark(
#   el_from_adj_mat(big_mat),
#   el_from_adj_mat(big_mat, .col_major = FALSE),
#   times = 5
#   # el_from_adj_mat(get_test_mat(.weighted = TRUE)),
#   # el_from_adj_mat(get_test_mat(.weighted = TRUE), .col_major = FALSE)
# )
# 
# el_from_adj_mat(get_test_mat(.weighted = TRUE))
# 
# get_test_nw(.bipartite = TRUE, .weighted = TRUE)
#  
# adj_mat <- get_test_mat(.weighted = TRUE)
# 
# edge_weights
# 
# nw <- adj_mat %>% 
#   network::as.network.matrix(directed = TRUE)
# 
# nw_el <- nw %>% 
#   network::as.matrix.network.edgelist() %>% 
#   .[ order(.[ , 1], .[ , 2] ) , ] %>% 
#   cbind(network::get.edge.attribute())
# %>% 
#   `attr<-`("vnames", NULL) %>% 
#   `attr<-`("n", NULL)
# 
# ig_el <- adj_mat %>% 
#   t() %>%
#   igraph::graph_from_adjacency_matrix(weighted = TRUE, mode = "directed") %>% 
#   igraph::as_data_frame() %>%
#   as.matrix() %>%
#   `dimnames<-`(NULL) %>% 
#   .[ order(.[ , 1], .[ , 2] ) , ]
# 
# ig_el %>% head()
# 
# # ig_el[ order(ig_el[ , 1], ig_el[ , 2] ) , ] %>% head()
# 
# melted <- matrix(c(col(adj_mat), row(adj_mat), as.vector(adj_mat)), ncol = 3)
# el <- melted[melted[, 3L] != 0, ]
# el %>% head()
# 
# all.equal(ig_el, el)
# 
# # 
# # nw_mat <- network::as.network.matrix(
# #   get_test_mat(.bipartite = TRUE), bipartite = TRUE, names.eval = "weight"
# # ) %>% 
# #   network::as.matrix.network(attrname = "weight")
# # 
# # ig_mat <- get_test_ig(.bipartite = TRUE) %>% 
# #   igraph::as_incidence_matrix(attr = NULL)
# # 
# # all.equal(nw_mat, ig_mat)
#   # igraph::graph_from_incidence_matrix()
# #   Matrix::isSymmetric()






