#' E-I Index
#' 
#' Given a categorical vertex attribute describing mutually exclusive groups, the E-I 
#' index represents a ratio of external to internal ties.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param vrt_attr `character` representing the name of a categorical vertex attribute present in `x`.
#' @param scope `character`. One of `"global"`, `"group"`, `"vertex"`, or `"full"`.
#' @param drop_loops `logical`. Whether loops should be removed prior to calculations. \cr
#'     Default: `FALSE`
#' 
#' @return something
#'  
#' @details
#' \deqn{E\mbox{-}I~Index = \frac{EL-IL}{EL+IL}}

#' 
#' @references Krackhardt, David, and Robert N. Stern. "Informal Networks and 
#' Organizational Crises: An Experimental Simulation." Social Psychology Quarterly 51, no.
#'  2 (1988): 123-40. \url{http://www.jstor.org/stable/2786835}.
#'  
#' @seealso [ei_index_global_permute()], [rep_mixing_matrix()], [network::mixingmatrix()]
#'  
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'   
#' @export
ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex"),
                     drop_loops = FALSE) {
  if (!class(x)[[1]] %in% c("sna_net", "igraph", "network", "tbl_graph")) {
    terminate(patch("`%s` must be an `sna_net`, `igraph`, `network`, or `tbl_graph`.",
              deparse(substitute(x))))
  }
  
  scope <- match.arg(scope, c("global", "groups", "vertices"))
  switch(scope,
         global = ei_index_global(x, vrt_attr, drop_loops = drop_loops),
         group = ei_index_group(x, vrt_attr, drop_loops = drop_loops),
         vertex = ei_index_vertex(x, vrt_attr, drop_loops = drop_loops))
}

ei_index_global <- function(x, vrt_attr, drop_loops = FALSE) {
  UseMethod("ei_index_global")
}

ei_index_global.igraph <- function(x, vrt_attr, drop_loops = FALSE) {
  if (vrt_attr == ".name") {
    terminate('".name" is not a valid vertex attribute.')
  }
  el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
  if(drop_loops && net_has_loops(x)) {
    el <- unique.matrix(el)
  }
  n_edges <- nrow(el)
  external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
  internal <- n_edges - external
  
  (external - internal) / n_edges
}

ei_index_global.network <- function(x, vrt_attr, drop_loops = FALSE) {
  if (vrt_attr == ".name") {
    terminate('".name" is not a valid vertex attribute.')
  }
  el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
  if(drop_loops && net_has_loops(x)) {
    el <- unique.matrix(el)
  }
  n_edges <- nrow(el)
  external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
  internal <- n_edges - external
  
  (external - internal) / n_edges
}

ei_index_global.adj_matrix <- function(x, drop_loops = FALSE) {
  # if (attr(x, "vrt_attr") == ".name") {
  #   terminate('".name" is not a valid vertex attribute.')
  # }
  el <- rep_as_edgelist(x, leave_raw = TRUE)
  if(drop_loops && net_has_loops(x)) {
    el <- unique.matrix(el)
  }
  n_edges <- nrow(el)
  external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
  internal <- n_edges - external
  
  (external - internal) / n_edges
}




ei_index_group <- function(x, vrt_attr, drop_loops = FALSE) {
  mix_mat <- rep_as_mixing_matrix(x, vrt_attr, drop_loops = drop_loops)
  internal <- diag(mix_mat)
  diag(mix_mat) <- NA_integer_
  external <- rowSums(mix_mat, na.rm = TRUE)
  
  ei <- (external - internal) / (external + internal)
  
  out <- data.frame(attribute = rownames(mix_mat),
                    external_ties = external,
                    internal_ties = internal,
                    ei_index = ei, 
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  class(out) <- c("ei_index_grp", "data.frame")
  out
}

ei_index_vertex <- function(x, vrt_attr, drop_loops = FALSE) {
  stop("Not yet implemented.")
  x
  vrt_attr
  drop_loops
}

# permute_matrix <- function(adj_matrix, test_fun, ...) {
#   n <- seq_len(nrow(adj_matrix))
#   force(test_fun)
#   
#   function() {
#     rownames(adj_matrix) <- rownames(adj_matrix)[sample(n)]
#     colnames(adj_matrix) <- rownames(adj_matrix)
#     test_fun(adj_matrix, ...)
#   }
# }

# adj_mat_tester <- igraph::graph("Zachary") %>% 
#   igraph::set_vertex_attr("name", value = c(letters, LETTERS)[seq_len(igraph::vcount(.))]) %>% 
#   igraph::as_adjacency_matrix(sparse = FALSE) %>% 
#   `class<-`(c("adj_matrix", "matrix"))
  
# adj_mat_tester
# permute_matrix(force)
# 
# adj_mat_permuter()
# 
# data(sampson, package = "ergm")

# rep_as_adj_matrix.igraph <- function(x) {
  # 
# }

# rep_as_edgelist.adj_matrix <- function(x) {
#   out <- as.data.frame.table(x, responseName = "n", 
#                              stringsAsFactors = FALSE)
#   out <- out[out[["n"]] > 0L, ]
#   out[["n"]] <- NULL
#   colnames(out) <- c(".ego", ".alter")
#   rownames(out) <- NULL
#   as_edge_data_frame(out)
# }
# 
# library(tidyverse)
# samplike %>% 
#   # as_igraph() %>% 
#   # igraph::as_adjacency_matrix()
#   network::set.vertex.attribute("vertex.names", 
#                                 network::get.vertex.attribute(samplike, "group")) %>% 
#   network::as.matrix.network() %>% 
#   permute_matrix() %>% 
#   rep_as_edgelist.adj_matrix() %>% 
#   network::network() %>% 
#   ei_index_global("group")
#   
#   
#   
#   permute_matrix() %>% 
#   as_tibble() %>% 
#   gather(var, val)
#   igraph::graph_from_edgelist() %>% 
  
