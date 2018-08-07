sna_as_network <- function(x) {
  UseMethod("sna_as_network")
}

sna_as_network.igraph <- function(ig) {
  graph_attrs <- sna_get_graph_attrs(ig)
  vert_attrs <- sna_get_vert_attrs(ig)
  edge_attrs <- sna_get_edge_attrs(ig)
  el <- sna_as_edgelist(ig)
  
  if("type" %in% names(vert_attrs)) {
    bipartite_arg <- length(which(!igraph::V(ig)$type))
    vert_attrs$type <- NULL
  } else {
    bipartite_arg <- FALSE
  }
  
  args <- list(n = unique(vapply(vert_attrs, length, integer(1), USE.NAMES = FALSE)),
               igraph::is_directed(ig),
               hyper = FALSE,
               loops = any(igraph::is.loop(ig)),
               multiple = any(igraph::is.multiple(ig)),
               bipartite = bipartite_arg)
  
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
  
  out
}


