#' Quadratic Assignment Procedure (Correlation)
#' 
#' @export
#' 
#' @examples
#' library(snatools)
#' 
#' data("florentine", package = "ergm")
#' 
#' res <- qap_cor(flomarriage, flobusiness)
#' 
#' res
#' 
#' autoplot(res)
#' 
qap_cor <- function(g1, g2, iterations = 1000L, diagonal = FALSE) {
  UseMethod("qap_cor")
}

#' @rdname qap_cor
#' 
#' @export
#' 
qap_cor.default <- function(x, y, iterations = NULL, diagonal = FALSE) {
  if(class(x) %in% c("igraph", "network")) {
    adj_mat1 <- rep_adjacency_matrix(x)
    if(!diagonal) {
      diag(adj_mat1) <- NA_integer_
    }
  } else if(class(x) == "matrix") {
    adj_mat1 <- x
  } else {
    stop("`x` must be of class `igraph`, `network`, or `matrix`.",
         call. = FALSE)
  }
  if(class(y) %in% c("igraph", "network")) {
    adj_mat2 <- rep_adjacency_matrix(y)
    if(!diagonal) {
      diag(adj_mat2) <- NA_integer_
    }
  } else if(class(y) == "matrix") {
    adj_mat2 == "matrix"
  } else {
    stop("`y` must be of class `igraph`, `network`, or `matrix`.",
         call. = FALSE)
  }
  if(nrow(adj_mat1) != nrow(adj_mat2)) {
    stop("`x`'s adjacency matrix has ", nrow(adj_mat1), " rows while `y`'s has ", nrow(adj_mat2), ".\n",
         "Do `x` and `y` have the same number of vertices?", call. = FALSE)
  }
  if(ncol(adj_mat1) != ncol(adj_mat2)) {
    stop("`x`'s adjacency matrix has ", ncol(adj_mat1), " columns while `y`'s has ", ncol(adj_mat2), ".\n",
         "Do `x` and `y` have the same number of vertices?", call. = FALSE)
  }
  
  adj_mat_pair <- list(adj_mat1, adj_mat2)
  class(adj_mat_pair) <- "adj_mat_pair"
  
  if(!length(iterations)) {
    iterations <- nrow(adj_mat1) * 500L
  }
  
  qap_cor(adj_mat_pair, iterations, diagonal)
}

#' @rdname qap_cor
#' 
#' @export
#' 
qap_cor.adj_mat_pair <- function(adj_mat_pair, iterations, diagonal = FALSE) {
  adj_mat1 <- adj_mat_pair[[1]]
  adj_mat2 <- adj_mat_pair[[2]]
  
  vectorized_mat1 <- as.vector(adj_mat1)
  test_rho <- cor(vectorized_mat1, as.vector(adj_mat2), use = "complete.obs")
  
  permuted_rhos <- vector("numeric", iterations)
  for(i in seq_len(iterations)) {
    permuted_matrix <- permute_matrix(adj_mat2)
    if(!diagonal) {
      diag(permuted_matrix) <- NA_integer_
    }
    permuted_rhos[[i]] <- cor(vectorized_mat1, as.vector(permuted_matrix), 
                              use = "complete.obs")
  }
  out <- list(observed_rho = test_rho,
              iterations = iterations,
              permuted_rhos = permuted_rhos,
              n_greater = length(which(permuted_rhos >= test_rho)),
              prop_greater = mean(as.numeric(permuted_rhos >= test_rho)),
              n_lesser = length(which(permuted_rhos < test_rho)),
              prop_lesser = mean(as.numeric(permuted_rhos < test_rho)))
  class(out) <- "qap_cor"
  
  out
}

#' @export
#' 
print.qap_cor <- function(x) {
  cat("observed rho:                    ", x$observed_rho, "\n")
  cat("\n")
  cat("# of iterations:                 ", x$iterations, "\n")
  cat("\n")
  cat("# permuted rhos >= observed rho: ", x$n_greater, " (", 
      round(x$prop_greater * 100, 2), "%)\n", sep = "")
  cat("# permuted rhos <= observed rho:  ", x$n_lesser, " (", 
      round(x$prop_lesser * 100, 2), "%)\n", sep = "")
}



#' @export
autoplot.qap_cor <- function(x) {
  check_ggplot()
  
  df <- data.frame(perm_rho = x$permuted_rhos)
  out <- ggplot2::ggplot(df) +
    ggplot2::stat_density(ggplot2::aes(perm_rho), fill = "orange", alpha = 0.5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = x$observed_rho, color = "Observed Correlation")) +
    ggplot2::guides(color = ggplot2::guide_legend(NULL)) +
    ggplot2::theme(plot.caption = ggplot2::element_text("mono"), legend.position = "top") +
    ggplot2::labs(x = paste("Correlations of Permuted Matrices"), y = "Density",
         title = "QAP Correlation", 
         subtitle = paste("Observed Correlation:", round(x$observed_rho, 3)),
         caption = paste("Iterations:", scales::comma(x$iterations))) +
    scale_x_ratio() +
    theme_sna()
  
  out
}


