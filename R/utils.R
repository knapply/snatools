permute_matrix <- function(x, out_class = "matrix") {
  n_row <- nrow(x)
  n_col <- ncol(x)
  row_seq <- sample(seq_len(nrow(x)))
  out <- matrix(x[row_seq, row_seq], nrow = n_row, ncol = n_col)
  rownames(out) <- rownames(x)
  colnames(out) <- colnames(x)
  class(out) <- out_class
  
  out
}

#' @export
#' 
drop_loops <- function(x) {
  UseMethod("drop_loops")
}

#' @export
#' 
drop_loops.igraph <- function(x) {
  igraph::simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
}

#' @export
#' 
drop_loops.network <- function(x) {
  out <- network::set.network.attribute(x, "loops", value = FALSE) # invisible
  out
}

#' @export
#' 
autoplot <- function(x, ...) {
  UseMethod("autoplot")
}

#' @export
#' 
theme_sna <- function(base_size = 11, base_family = "serif", legend.position = "top") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  out <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
  out <- out + ggplot2::theme(legend.position = legend.position)
  
  out
}

#' @export
#' 
scale_x_ratio <- function(limits = c(-1, 1)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  ggplot2::scale_x_continuous(limits = limits)
}

#' @export
#' 
scale_y_ratio <- function(limits = c(-1, 1)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  ggplot2::scale_y_continuous(limits = limits)
}
  
