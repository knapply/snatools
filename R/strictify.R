#' Clean a graph object's attributes in a standardized fashion.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A cleaned version of `x`.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data("Koenigsberg", package = "igraphdata")
#' 
#' Koenigsberg %>% vrt_get_attrs()
#' 
#' Koenigsberg %>% strictify() %>% strictify()
#' 
#' 
#' @export
strictify <- function(x) {
  UseMethod("strictify")
}

#' @rdname strictify
#' 
#' @export
#' 
strictify.igraph <- function(ig) {
  graph_attrs <- net_get_attrs(ig)
  edge_attrs <- edg_get_attrs(ig)
  vert_attrs <- vrt_get_attrs(ig)
  # names(vert_attrs)[names(vert_attrs) == "vertex.names"] <- "name"
  
  igraph::graph_attr(ig) <- graph_attrs
  igraph::edge_attr(ig) <- edge_attrs
  igraph::vertex_attr(ig) <- vert_attrs
  
  ig
}

#' @rdname strictify
#' 
#' @export
#' 
strictify.network <- function(nw) {
  if(is.null(nw$gal$bipartite)) { # may not exist
    nw$gal$bipartite <- FALSE
  }
  vert_attrs <- vrt_get_attrs(nw)
  names(vert_attrs)[names(vert_attrs) == "name"] <- "vertex.names"
  graph_attrs <- net_get_attrs(nw)
  edge_attrs <- edg_get_attrs(nw)
  el <- rep_as_edgelist(nw)
  
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
