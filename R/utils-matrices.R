.throw_non_matrix <- function(.x) {
  if (!is.matrix(.x) & !inherits(.x, "Matrix")) {
    .stop("`", substitute(.x), "` is not a `matrix` or `Matrix`.")
  }
}

.throw_invalid_adj_mat <- function(adj_mat, directed, allow_loops, allow_weights) {
  res <- is_valid_adj_mat(
    adj_mat,
    directed = directed,
    allow_loops = allow_loops,
    allow_weights = allow_weights
  )

  if (!res) {
    .stop("`", substitute(adj_mat), "` is not a valid adjacency matrix.")
  }
}

.throw_invalid_el <- function(edge_list, directed, allow_loops,
                              allow_weights, allow_parallel_edges) {
  res <- is_valid_el(
    edge_list,
    directed = directed,
    allow_loops = allow_loops,
    allow_weights = allow_weights,
    allow_parallel_edges = allow_parallel_edges
  )

  if (!res) {
    .stop("`", substitute(edge_list), "` is not a valid edge list.")
  }
}


adj_mat_is_weighted <- function(adj_mat, ...) {
  .throw_non_matrix(adj_mat)

  any(!adj_mat %in% c(0, 1))
}


adj_mat_is_square <- function(adj_mat, ...) {
  .throw_non_matrix(adj_mat)

  dims <- dim(adj_mat)
  dims[[1L]] == dims[[2L]]
}


adj_mat_has_loops <- function(adj_mat, ...) {
  .throw_non_matrix(adj_mat)

  any(adj_mat != 0)
}


is_valid_adj_mat <- function(adj_mat, directed, allow_loops, allow_weights,
                             .diagnostic = TRUE, ...) {
  .throw_non_matrix(adj_mat)

  if (!adj_mat_is_square(adj_mat)) {
    if (.diagnostic) {
      message("`adj_mat` is not a square matrix.",
              sprintf("\n\t- # rows: %s\n\t- # cols: %s", nrow(adj_mat), ncol(adj_mat)))
    }

    return(FALSE)
  }

  if (!directed && !Matrix::isSymmetric(adj_mat)) {
    if (.diagnostic) {
      message("`directed` = `FALSE`, but `adj_mat` is not symmetric.")
    }

    return(FALSE)
  }

  if (!allow_loops && adj_mat_has_loops(adj_mat)) {
    if (.diagnostic) {
      message("`loops` is `FALSE` but `adj_mat`'s diagonal contains non-zero values")
    }

    return(FALSE)
  }

  if (!allow_weights && adj_mat_is_weighted(adj_mat)) {
    if (.diagnostic) {
      message("`weights` is `FALSE`, but `adj_mat` contains values that are not 0 or 1")
    }

    return(FALSE)
  }

  TRUE
}



el_is_weighted <- function(edge_list, ...) {
  .throw_non_matrix(edge_list)

  is.numeric(el) && nrow(el) > 2L
}


el_has_loops <- function(edge_list, ...) {
  .throw_non_matrix(edge_list)

  any(edge_list[, 1L] == edge_list[, 2L])
}


el_has_parallel_edges <- function(edge_list, directed = TRUE, ...) {
  .throw_non_matrix(edge_list)

  if (directed) {
    anyDuplicated(edge_list) != 0L
  } else {
    anyDuplicated(t(apply(edge_list, 1L, sort)) == 0L)
  }
}


el_is_sequential <- function(edge_list, ...) {
  .throw_non_matrix(edge_list)

  all(seq_along(edge_list[, c(1L, 2L)]) %in% edge_list[, c(1L, 2L)])
}


is_valid_el <- function(edge_list, directed, allow_loops, allow_weights,
                        allow_parallel_edges = FALSE,
                        .diagnostic = TRUE, ...) {
  .throw_non_matrix(edge_list)

  if (!ncol(edge_list) %in% c(2L, 3L)) {
    if (.diagnostic) {
      message("`edge_list` does not have 2 or 3 columns.")
    }

    return(FALSE)
  }

  if (!allow_weights && el_is_weighted(edge_list)) {
    if(.diagnostic) {
      message("`allow_weights` is `FALSE`, but `edge_list` has 3 columns.")
    }

    return(FALSE)
  }

  if (!allow_loops && el_has_loops(edge_list)) {
    if (.diagnostic) {
      .stop("`allow_loops` is `FALSE`, but `edge_list` contains loops.")
    }

    return(FALSE)
  }

  if (is.numeric(edge_list) && !el_is_sequential(edge_list)) {
    if (.diagnostic) {
      message("`edge_list` contains non-sequential node indices.")
    }

    return(FALSE)
  }

  if (!allow_parallel_edges && el_has_parallel_edges(edge_list, directed = directed)) {
    if (.diagnostic) {
      message("`allow_parallel_edges` is `FALSE`, but `edge_list` has parallel edges.")
    }

    return(FALSE)
  }

  TRUE
}


