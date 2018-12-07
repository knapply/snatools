#' Permute matrix columns and rows.
#' 
#' @export
#' 
permute_matrix <- function(x, ...) {
  UseMethod("permute_matrix")
}

#' @rdname permute_matrix
#' 
#' @export
#' 
permute_matrix.igraph <- function(x, matrix_fun = rep_adjacency_matrix, ...) {
  if(!is.function(matrix_fun)) {
    stop("`matrix_fun` is not a function.")
  }
  matrix_fun <- match.fun(matrix_fun)
  mat <- matrix_fun(x, ...)
  permute_matrix(mat)
}

#' @rdname permute_matrix
#' 
#' @export
#' 
permute_matrix.network <- function(x, matrix_fun = rep_adjacency_matrix, ...) {
  if(!is.function(matrix_fun)) {
    stop("`matrix_fun` is not a function.")
  }
  matrix_fun <- match.fun(matrix_fun)
  mat <- matrix_fun(x, ...)
  permute_matrix(mat)
}

#' @rdname permute_matrix
#' 
#' @export
#' 
permute_matrix.matrix <- function(x, out_class = class(x)) {
  n_row <- nrow(x)
  n_col <- ncol(x)
  random_row_seq <- sample(seq_len(nrow(x)))
  random_col_seq <- sample(seq_len(ncol(x)))
  out <- matrix(x[random_row_seq, random_col_seq], nrow = n_row, ncol = n_col)
  rownames(out) <- rownames(x)
  colnames(out) <- colnames(x)
  class(out) <- out_class
  
  out
}


