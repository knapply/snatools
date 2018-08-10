#' Convert a graph object into a `network` object.
#' 
#' @param x A graph object.`igraph` objects are supported.
#' 
#' @return A `network` object.
#' 
#' @export
as_network <- function(x) {
  UseMethod("as_network")
}

#' @rdname as_network
#' 
#' @export
#' 
as_network.igraph <- function(x) {
  graph_attrs <- net_get_attrs(x)
  vert_attrs <- vrt_get_attrs(x)
  edge_attrs <- edg_get_attrs(x)
  el <- rep_as_edgelist(x)
  
  if("type" %in% names(vert_attrs)) {
    bipartite_arg <- length(which(!igraph::V(x)$type))
    vert_attrs$type <- NULL
  } else {
    bipartite_arg <- FALSE
  }
  
  args <- list(n = unique(vapply(vert_attrs, length, integer(1), USE.NAMES = FALSE)),
               igraph::is_directed(x),
               hyper = FALSE,
               loops = any(igraph::is.loop(x)),
               multiple = any(igraph::is.multiple(x)),
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

#' @rdname as_network
#' 
#' @export
#' 
as_network.default <- function (x) {
    tryCatch({
        network::as.network(x)
      }, error = function(e) stop("Objects of ", class(x)[[1]],
        " are not supported at this time.", call. = FALSE))
}

#' @rdname as_network
#' 
#' @export
#' 
as_network.network <- function(x) {
  x
}

