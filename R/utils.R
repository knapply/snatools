

clean_network_metadata <- function(x) {
  if(class(x) != "network") {
    stop("`fill_network_metadata()` is only applicable to `network` objects.")
  }
  if(is.null(x$gal$bipartite)) {
    network::set.network.attribute(x, "bipartite", network::is.bipartite(x))
  }
  if(is.null(x$gal$loops)) {
    network::set.network.attribute(x, "loops", network::has.loops(x))
  }
  if(is.null(x$gal$hyper)) {
    network::set.network.attribute(x, "hyper", network::is.hyper(x))
  }
  if(is.null(x$gal$multiple)) {
    if(is.null(network::is.multiplex(x))) {
      network::set.network.attribute(x, "multiple", FALSE)
    } else {
    network::set.network.attribute(x, "multiple", network::is.multiplex(x))
    }
  }
  net_attrs <- net_get_attrs(x, drop_metadata = FALSE)
  net_attrs <- net_attrs[order(names(net_attrs))]
  for(i in names(net_attrs)) {
    network::delete.network.attribute(x, i)
    x$gal[[i]] <- NULL
  }
  for(i in seq_along(net_attrs)) {
    network::set.network.attribute(x, names(net_attrs)[[i]], net_attrs[[i]])
  }
  x
}

#' Permute matrix columns and rows.
#' 
#' @export
#' 
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

# permute_matrix <- function(x, out_class = "matrix") {
#   X <- Matrix::Matrix(x)
#   n_row <- nrow(x)
#   n_col <- ncol(x)
#   row_seq <- sample(seq_len(nrow(x)))
#   out <- matrix(x[row_seq, row_seq], nrow = n_row, ncol = n_col)
#   # X[row_seq, row_seq]
#   matrix(x[row_seq, row_seq], nrow = n_row, ncol = n_col)
#   
#   rownames(out) <- rownames(x)
#   colnames(out) <- colnames(x)
#   class(out) <- out_class
#   
#   out
# }


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
  
