sna_standardize_graph <- function(x) {
  UseMethod("sna_standardize_graph")
}

sna_standardize_graph.igraph <- function(ig) {
  graph_attrs <- sna_get_graph_attrs(ig)
  edge_attrs <- sna_get_edge_attrs(ig)
  vert_attrs <- sna_get_vert_attrs(ig)
  names(vert_attrs)[names(vert_attrs) == "vertex.names"] <- "name"
  
  igraph::graph_attr(ig) <- graph_attrs
  igraph::edge_attr(ig) <- edge_attrs
  igraph::vertex_attr(ig) <- vert_attrs
  
  ig
}

sna_standardize_graph.network <- function(nw) {
  vert_attrs <- sna_get_vert_attrs(nw)
  names(vert_attrs)[names(vert_attrs) == "name"] <- "vertex.names"
  graph_attrs <- sna_get_graph_attrs(nw)
  edge_attrs <- sna_get_edge_attrs(nw)
  el <- sna_as_edgelist(nw)
  
  args <- list(n = nw$gal$n,
               directed = nw$gal$directed,
               hyper = nw$gal$hyper,
               loops = nw$gal$loops,
               multiple = nw$gal$multiple,
               bipartite = nw$gal$bipartite)
  
  out <- do.call(network::network.initialize, args)
  
  if(nrow(el)) {
    network::add.edges(out, tail = el[, 1], head = el[, 2]) # assigns invisibly
  }
  if(length(edge_attrs)) {
    for(e_attr in names(edge_attrs)) {
      network::set.edge.attribute(out, e_attr, edge_attrs[[e_attr]]) # assigns invisibly
    }
  }
  if(length(vert_attrs)){
    for(v_attr in names(vert_attrs)){
      network::set.vertex.attribute(out, v_attr, vert_attrs[[v_attr]]) # assigns invisibly
    }
  }
  if(length(graph_attrs)){
    for(g_attr in names(graph_attrs)) {
      network::set.network.attribute(out, g_attr, graph_attrs[[g_attr]]) # assigns invisibly
    }
  }
  
  if(is.numeric(out$gal$bipartite)) {
    out$gal$loops <- FALSE
  }

  out
}
