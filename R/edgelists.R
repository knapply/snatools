#' Get the edge list representation of a graph as a matrix.
#' 
# @template param-net
# @template param-node_attr
# @template param-use_node_names
# @template bknapp-author
#' 
#' @export
# fetch_edgelist <- function(.net, .use_node_names = TRUE, .node_attr = NULL, ...) {
#   stopifnot(is.logical(.use_node_names))
#   stopifnot(.is_null(.node_attr) || is.character(.node_attr))
#   
#   if (!.is_null(.node_attr)) {
#     fill <- get_vert_attr(.g, .node_attr)
#   } else if (.use_node_names) {
#     fill <- get_vert_names(.net)
#   } else {
#     fill <- NULL
#   }
#   
#   out <- get_el(.net)
#   
#   if (.is_null(fill)) {
#     return(out)
#   }
#   
#   matrix(fill[out], ncol = 2L)
# }
# 
# 
# 
.fetch_edgelist <- function(.g) {
  UseMethod(".fetch_edgelist")
}

#' @importFrom igraph as_edgelist
.fetch_edgelist.igraph <- function(.net) {
  as_edgelist(graph = .net, names = FALSE)
}

.fetch_edgelist.network <- function(.net) {
  out <- cbind(.map_num(.net[["mel"]], `[[`, "outl"),
               .map_num(.net[["mel"]], `[[`, "inl"), 
               deparse.level = 0)
  
  if (net_is_directed(.net)) {
    return(out)
  }
  .sort_edge_indices(out)
}



# network::as.matrix.network.edgelist