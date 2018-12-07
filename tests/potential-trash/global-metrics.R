calc_max_edges <- function(x, loops = FALSE) {
  n_vertices <- net_count_vertices(x)
  if (n_vertices == 0L) {
    return(0L)
  }
  if (n_vertices == 1L) {
    if (loops) {
      return(1L)
    }
    return(0L)
  }
  loops <- as.integer(loops)
  switch(if (net_is_bipartite(x)) "bip" else if (net_is_directed(x)) "dir" else "undir",
         bip = net_count_actors(x) * (n_vertices - net_count_actors(x)),
         dir = n_vertices * (n_vertices - 1L) + loops,
         undir = 0.5 * (n_vertices * (n_vertices - 1L) + loops))
}

#' Calculate a graph object's density.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, or`tbl_graph`, or `edgelist` object.
#' 
#' @return `double` indicating the ratio of existing edges to maximum potential edges.
#'
#' @seealso [igraph::edge_density()], [sna::gden()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' ig <- c(0, 1, 0, 1,
#'         1, 0, 0, 0,
#'         0, 1, 0, 1,
#'         1, 0, 1, 0) %>% 
#'   matrix(nrow = 4, byrow = TRUE) %>% 
#'   igraph::graph_from_adjacency_matrix()
#'   
#' nw <- ig %>% 
#'   as_network()
#'  
#' ig %>% 
#'   calc_net_density()
#'   
#' ig %>% 
#'   net_as_undirected() %>% 
#'   calc_net_density()
#' 
#' nw %>% 
#'   calc_net_density()
#' 
#' nw %>% 
#'   net_as_undirected() %>% 
#'   calc_net_density()
#' 
#' @export
calc_net_density <- function(x) {
  net_count_edges(x) / calc_max_edges(x)
}