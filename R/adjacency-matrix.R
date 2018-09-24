#' Construct a graph object's adjacency matrix representation.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, or`tbl_graph`, or `edgelist` object.
#' @param use_names `logical` (default: `TRUE`) indicating whether to use vertex names for
#' row and column names.
#' @param vrt_attr `character` (default: `NULL`) indicating which vertex attribute to use 
#' in for row and column names. If provided, `vrt_attr` overrides `use_names`.`
#' @param sparse `logical` (default: `TRUE`) indicating whether to use 
#' `Matrix::sparseMatrix()` in constructing the adjacency matrix.
#' 
#' @return An `adj_matrix` object.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' ig <- igraph::graph("Krackhardt Kite") %>% 
#'   igraph::set_vertex_attr("name", value = paste0("vrt_",
#'                                                   seq_len(igraph::vcount(.)))) %>% 
#'   igraph::set_vertex_attr("group", value = sample(letters, igraph::vcount(.))) %>% 
#'   igraph::set_vertex_attr("group", value = sample(letters, igraph::vcount(.)))
#'   
#' rep_as_adj_matrix(ig)
#' 
#' rep_as_adj_matrix(ig, vrt_attr = "group")
#'   
#' nw <- as_network(ig)
#' 
#' rep_as_adj_matrix(nw, use_names = FALSE)
#' 
#' @importFrom Matrix forceSymmetric isSymmetric Matrix sparseMatrix
#' @export
rep_as_adj_matrix <- function(x, use_names = TRUE, vrt_attr = NULL, sparse = TRUE) {
  validate_graph(x)
  if (net_is_multiplex(x)) {
    terminate(patch("`%s` is multiplex. Filter `%s`'s edges before calling 
                    `rep_as_adj_matrix()`", 
                    deparse(substitute(x)), deparse(substitute(x))))
  }
  if (use_names && is.null(vrt_attr)) {
    vrt_attr <- get_vrt_names_attr(x)
    adj_mat_type <- "vrt_names"
  }
  if (!is.null(vrt_attr)) {
    validate_vrt_attr(x, vrt_attr)
    adj_mat_type <- "vrt_attrs"
    dim_names <- vrt_get_attr(x, vrt_attr)
  } else {
    adj_mat_type <- "vrt_indices"
    dim_names <- NULL
  }
  n_vertices <- net_count_vertices(x)
  el <- rep_as_edgelist(x, use_names = FALSE, leave_raw = TRUE)
  if (sparse) {
    fill <- rep(1L, nrow(el))
    out <- sparseMatrix(i = pmin.int(el[, ".ego"], el[, ".alter"]),
                        j = pmax.int(el[, 1], el[, 2]),
                        x = fill,
                        symmetric = !net_is_directed(x),
                        dims = c(n_vertices, n_vertices))
  } else {
    out <- Matrix(integer(n_vertices^2), nrow = n_vertices, ncol = n_vertices,
                  sparse = FALSE)
    out[el] <- 1L
    if (!net_is_directed(x) && !isSymmetric(out)) {
      out <- forceSymmetric(out)
    }
  }
  if (!is.null(dim_names)) {
    dimnames(out) <- list(dim_names, dim_names)
  }
  out <- as.matrix(out)
  attr(out, "adj_mat_type") <- adj_mat_type
  if (adj_mat_type == "vrt_attrs") {
    attr(out, "vrt_attr_name") <- vrt_attr
  }
  out <- set_metadata_attr(out, x)
  class(out) <- c("adj_matrix", "matrix")
  out
}

#' @rdname rep_as_adj_matrix
#' 
#' @export
print.adj_matrix <- function(x) {
  n_col <- ncol(x)
  cat_patch("# An adj_matrix: %sx%s", nrow(x), n_col)
  cat("\n")
  cat_patch("# Suppressing column names.")
  # cat("\n")
  # rownames(x) <- paste0(txt_extract(rownames(x), "^.{5}"), ".")
  colnames(x) <- rep("", n_col)
  class(x) <- "matrix"
  for (i in names(attributes(x))) {
    if (!i %in% c("dim", "dimnames")) {
        attr(x, i) <- NULL
      }
    }
  print(x)
}

# 
# test_mat <- test_ig %>% 
#   rep_as_adj_matrix()
# 
# raw <- test_mat %>% 
#   `class<-`("matrix")
#   
# build_matrix_permuter <- function(x) {
#   n <- seq_len(attr(x, "n_vertices"))
#   function() {
#     new_labs <- rownames(x)[sample(n)]
#     sum(`dimnames<-`(x, list(new_labs, new_labs)))
#   }
# }
# 
# permuter <- build_matrix_permuter(test_mat)
# 
# permute_matrix2 <- function(x) {
#   new_seq <- sample(seq_len(attr(x, "n_vertices")))
#   new_labs <- rownames(x)[new_seq]
#   sum(`dimnames<-`(x, list(new_labs, new_labs)))
# }
# 
# microbenchmark::microbenchmark(
#   replicate(5000, permute_matrix2(test_mat)),
#   replicate(5000, permuter()),
#   times = 1
# )
# 
# 
# 
# igraph::graph("Zachary") %>% 
#   rep_as_adj_matrix(use_names = )
# 
# test_ig <- build_test_graph("ig", n_nodes = 1000) %>% 
#   igraph::simplify()
# 
# test_nw <- test_ig %>% 
#   as_network()
# 
# 
# microbenchmark::microbenchmark(
#   network::as.matrix.network.adjacency(test_nw),
#   igraph::as_adjacency_matrix(test_ig, sparse = FALSE),
#   rep_as_adj_matrix(test_ig)
# )
# 
# test_res <- test_ig %>% 
#   igraph::simplify() %>% 
#   rep_as_adj_matrix()
# 
# rep_as_adj_matrix(test) %>% 
#   isSymmetric()
# 
# 
# test <- igraph::graph("Zachary")
# igraph::V(test)$name <- seq_len(igraph::vcount(test))
# 
# test %>% 
#   rep_as_adj_matrix()
#   %>% 
#   igraph::as_adjacency_matrix(sparse = T) %>% 
#   as.matrix() %>% 
#   isSymmetric()
# 
# sampson_monastery %>% 
#   as_igraph() %>% 
#   edg_filter(time == 1, relation == "liking") %>% 
#   # rep_as_edgelist()
#   # igraph::as.undirected() %>% 
#   # igraph:::get.adjacency.dense() %>% 
#   rep_as_adj_matrix()
#   isSymmetric()

# network::as.matrix.network(as_network(sampson_monastery))
# 
# sampson_monastery




# sampson_monastery %>% 
# samplike %>% 
#   as.matrix() %>% 
#   # as_network() %>% 
#   rep_as_adj_matrix("relation", "liking")
# 
# all.equal(
#   network::as.matrix.network(samplike) %>% is.double()
#   rep_as_adj_matrix(samplike) %>% is.integer()
# )
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
