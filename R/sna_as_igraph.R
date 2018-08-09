#' Convert `network` objects to `igraph`.
#' 
#' @export
#' 
sna_as_igraph <- function(x) {
  UseMethod("sna_as_igraph")
}

#' @rdname sna_as_igraph
#' 
#' @export
#' 
sna_as_igraph.network <- function(nw) {
  if (nw$gal$hyper) {
    stop("Hypergraphs are not supported.", call. = FALSE)
  }
  graph_attrs <- sna_get_graph_attrs(nw)
  
  if(is.numeric(nw$gal$bipartite)) {
    graph_attrs$loops <- NULL
    false_nodes <- seq_len(nw$gal$bipartite)
    true_nodes <- seq.int(nw$gal$bipartite + 1, nw$gal$n)
    network::set.vertex.attribute(nw, "type", value = FALSE, v = false_nodes)
    network::set.vertex.attribute(nw, "type", value = TRUE, v = true_nodes)
  }
  vert_attrs <- sna_get_vert_attrs(nw)
  edge_attrs <- sna_get_edge_attrs(nw)
  el <- sna_get_edgelist(nw)
 
  out <- igraph::graph_from_edgelist(el, directed = nw$gal$directed)
  igraph::graph_attr(out) <- graph_attrs
  igraph::edge_attr(out) <- edge_attrs
  igraph::vertex_attr(out) <- vert_attrs
  
  sna_clean_graph(out)
}
