adj_mat_as_el <- function(adj_mat, directed = TRUE, allow_loops = FALSE,
                          allow_weights = FALSE, ...) {
  .throw_invalid_adj_mat(
    adj_mat,
    directed = directed,
    allow_loops = allow_loops,
    allow_weights = allow_weights
  )

  if (allow_weights && adj_mat_is_weighted(adj_mat)) {
    as.matrix(Matrix::summary(as(adj_mat, "dgCMatrix")))
  } else {
    as.matrix(Matrix::summary(as(adj_mat, "dgCMatrix")))[, c(1L, 2L)]
  }
}
