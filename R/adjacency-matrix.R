# rep_as_adj_matrix <- function(x) {
#   UseMethod("rep_as_adj_matrix")
# }
# 
# 
# 
# 
# x <- build_test_graph("ig") %>% 
#   rep_as_edgelist(use_names = FALSE)
# 
# 
# 
# rep_as_adj_matrix.edgelist <- function(x) {
#   el <- as.matrix(x)
#   n_vertices <- attr(x, "n_vertices")
#   if (attr(x, "is_directed")) {
#     
#   }
#   fill <- rep.int(1L, dim(el)[[1L]])
#   out <- Matrix::sparseMatrix(dims = c(n_vertices, n_vertices), 
#                               i = el[, 1], j = el[, 2],
#                               x = fill)
#   out
# }
# 
# snatools:::build_test_graph("ig") %>% 
#   snatools:::rep_as_edgelist(use_names = FALSE) %>% 
#   rep_as_adj_matrix.edgelist()
#   
# snatools:::rep_as_edgelist() %>% 
#   attributes()
# 
# rep_as_adj_matrix.igraph <- function(x) {
#   mat_dims <- net_count_vertices(x)
#   dim_names <- vrt_get_attr(x, "name")
#   el <- snatools:::rep_as_edgelist(x, use_names = FALSE, leave_raw = TRUE)
#   out <- Matrix::Matrix(0L, nrow = mat_dims, ncol = mat_dims, sparse = TRUE)
#   out[el] <- 1L
#   dimnames(out) <- list(dim_names, dim_names)
#   out
# }
# 
# 
# library(snatools)
# # big_test <- igraph::random.graph.game(100000, p.or.m = 0.5)
# mine <- snatools:::build_test_graph("ig") %>%
#   rep_as_adj_matrix.igraph()
# 
# theirs <- snatools:::build_test_graph("ig") %>%
#   igraph:::get.adjacency.sparse
# 
# # big_test %>% 
#   snatools:::rep_as_edgelist(use_names = FALSE) %>% 
#   rep_as_adj_matrix.edgelist() 
#   # Matrix::Matrix()
#   # as.matrix()
# 
# # el  <- cbind(a=1:5, b=5:1) #edgelist (a=origin, b=destination)
# mat <- matrix(0, 
#               length(unique(as.vector(el, mode = "integer"))), 
#               length(unique(as.vector(el, mode = "integer"))))
# mat[el] <- 1
# mat
# 
# 
# 
# 
# 
# microbenchmark::microbenchmark(
#   length(unique(c(x))), 
#   length(unique(as.vector(x, mode = "integer"))),
#   length(unique(`dim<-`(as.matrix(x), NULL)))
#   )
# c(x) %>% is.integer()
# vectorize_mat <- function(x) {
#   length(unique(`dim<-`(as.matrix(x), NULL)))
# }
# 
# 
# 
