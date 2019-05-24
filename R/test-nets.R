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

get_test_ig <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, 
                        .diag = FALSE, ...) {
  mat <- get_test_mat(.directed = .directed, .bipartite = .bipartite,
                      .weighted = .weighted, .diag = .diag, ...)
  .weighted <- .weighted %{F}% NULL

  if (.bipartite) {
    igraph::graph_from_incidence_matrix(mat, directed = .directed, weighted = .weighted)
  } else {
    igraph::graph_from_adjacency_matrix(mat, weighted = .weighted, diag = .diag)
  }
}

# get_test_ig(.directed = F, .bipartite = T, .weighted = T, .diag = F)
# 
# get_test_mat(.directed = TRUE, .bipartite = TRUE, .weighted = TRUE) %>%
#   # igraph::graph_from_incidence_matrix()
#   Matrix::isSymmetric()
