ei_index_global_permute2 <- function(x, vrt_attr, iterations = 1000L, diagonal = FALSE) {
  if(!class(x) %in% c("igraph", "network")) {
    stop("`x` must be of class `igraph` or `network`.", call. = FALSE)
  }
  observed_ei <- ei_index_global(x, vrt_attr)            # value test
  attr_adj_mat <- rep_attr_adj_mat(x, vrt_attr)    # matrix to permute
  permuted_eis <- vector("double", iterations)        # initialize storage vector

  if(net_is_directed(x)) {
    if(diagonal) { # seperate loops to prevent need to check `diagonal` every iteration
      for(i in seq_len(iterations)) {
        class(attr_adj_mat) <- "sociomatrix.sna"
        permuted_matrix <- sna::rmperm(attr_adj_mat)
        rownames(permuted_matrix) <- rownames(attr_adj_mat)
        colnames(permuted_matrix) <- colnames(attr_adj_mat)
        class(permuted_matrix) <- "attr_adj_mat"
        permuted_attr_el <- rep_attr_el(permuted_matrix)
        permuted_eis[[i]] <- ei_index_global(permuted_attr_el)
      }
    } else {
      for(i in seq_len(iterations)) {
        class(attr_adj_mat) <- "sociomatrix.sna"
        permuted_matrix <- sna::rmperm(attr_adj_mat)
        rownames(permuted_matrix) <- rownames(attr_adj_mat)
        colnames(permuted_matrix) <- colnames(attr_adj_mat)
        diag(permuted_matrix) <- NA_integer_
        class(permuted_matrix) <- "attr_adj_mat"
        permuted_attr_el <- rep_attr_el(permuted_matrix)
        permuted_eis[[i]] <- ei_index_global(permuted_attr_el)
      }
    }
  } else {
    for(i in seq_len(iterations)) {
      permuted_matrix <- sna::rmperm(attr_adj_mat)
      rownames(permuted_matrix) <- rownames(attr_adj_mat)
      colnames(permuted_matrix) <- colnames(attr_adj_mat)
      permuted_matrix[upper.tri(permuted_matrix, diag = !diagonal)] <- NA_integer_
      class(permuted_matrix) <- "attr_adj_mat"
      permuted_attr_el <- rep_attr_el(permuted_matrix)
      permuted_eis[[i]] <- ei_index_global(permuted_attr_el)
    }
  }
  out <- list(vrt_attr = vrt_attr,
              observed_ei = observed_ei,
              iterations = iterations,
              permuted_eis = permuted_eis,
              n_greater = length(which(permuted_eis >= observed_ei)),
              prop_greater = mean(as.numeric(permuted_eis >= observed_ei)),
              n_lesser = length(which(permuted_eis < observed_ei)),
              prop_lesser = mean(as.numeric(permuted_eis < observed_ei)))
  class(out) <- "ei_index_global_permute"

  out
}

data("samplk", package = "ergm")
x <- samplk1
set.seed(1234)
res1 <- ei_index_global_permute(samplk1, "group")
set.seed(1234)
res2 <- ei_index_global_permute(samplk1, "group", perm_fun = permute_matrix_labs)
set.seed(1234)
res3 <- ei_index_global_permute2(samplk1, "group")

autoplot(res1)
autoplot(res2)
autoplot(res3)

bench::mark(
  ei_index_global_permute(samplk1, "group", iterations = 50, perm_fun = permute_matrix),
  
  ei_index_global_permute2(samplk1, "group", iterations = 50),
  check = FALSE, iterations = 10
)
