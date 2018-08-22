#' Clean a graph object's internals and standardize attributes.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A cleaned version of `x` with safe attributes.
#' 
#' @seealso [as_igraph()], [as_network()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data("Koenigsberg", package = "igraphdata")
#' 
#' Koenigsberg %>% vrt_attrs()
#' 
#' Koenigsberg %>% clean_graph()
#' 
#' 
#' @export
clean_graph <- function(x, ...) {
  UseMethod("clean_graph")
}

#' @rdname clean_graph
#' 
#' @export
#' 
clean_graph.igraph <- function(x, actor_type = TRUE) {
  graph_attrs <- net_attrs(x)
  if(!"loops" %in% names(graph_attrs)) {
    graph_attrs$loops <- any(igraph::is.loop(x))
  }
  if("type" %in% vrt_attr_names(x)) {
    true_nodes <- which(igraph::V(x)$type)
    false_nodes <- which(!igraph::V(x)$type)
    if(actor_type) {
      new_node_order <- c(true_nodes, false_nodes)
    } else {
      new_node_order <- c(false_nodes, true_nodes)
    }
    x <- igraph::permute(x, match(seq_along(new_node_order), new_node_order))
  }
  graph_attrs <- graph_attrs[order(names(graph_attrs))]
  edge_attrs <- edg_attrs(x)
  if(length(edge_attrs)) {
    edge_attrs <- edge_attrs[order(names(edge_attrs))]
  }
  vert_attrs <- vrt_attrs(x)
  if(length(vrt_attrs)) {
    vert_attrs <- vert_attrs[order(names(vert_attrs))]
    names(vert_attrs)[names(vert_attrs) == "vertex.names"] <- "name"
  }
  
  if(length(graph_attrs)) {
    igraph::graph_attr(x) <- graph_attrs
  }
  if(length(edge_attrs)) {
    igraph::edge_attr(x) <- edge_attrs
  }
  if(length(vert_attrs)) {
    igraph::vertex_attr(x) <- vert_attrs
  }
  
  x
}

#' @rdname clean_graph
#' 
#' @export
#' 
clean_graph.network <- function(x, ...) {
  if(is.null(x$gal$bipartite)) { # may not exist
    x$gal$bipartite <- FALSE
  }
  vert_attrs <- vrt_attrs(x)
  names(vert_attrs)[names(vert_attrs) == "name"] <- "vertex.names"
  vert_attrs <- vert_attrs[order(names(vert_attrs))]
  graph_attrs <- net_attrs(x)
  graph_attrs <- graph_attrs[order(names(graph_attrs))]
  edge_attrs <- edg_attrs(x)
  edge_attrs <- edge_attrs[order(names(edge_attrs))]
  # out[order(names(out))]
  el <- rep_edgelist(x)
  
  args <- list(n = x$gal$n,
               directed = x$gal$directed,
               hyper = x$gal$hyper,
               loops = x$gal$loops,
               multiple = x$gal$multiple,
               bipartite = x$gal$bipartite)
  
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
