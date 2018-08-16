#' Convert `igraph` objects to `network`.
#' 
#' @param x An graph object.`igraph` objects are currently supported.
#' @param actor_type `logical`. The vertex `"type"` specifying which
#' vertices in `x` are to be treated as "actors". See \href{#details}{__Details__} \cr
#' 
#' @details `actor_type`. By default, `igraph` represents bipartite graphs using vertex 
#' attribute named `"type"`. `network` represents bipratite graphs through a count of the 
#' vertices in the "actors" partition. `actor_type` provides a way to specify whether the
#' preceding `igraph` object's `TRUE` or `FALSE` nodes should be represented as the "actors" 
#' in the resulting `network` object (as opposed to the vertices of the second partition,
#' e.g. "events").
#' 
#' @return A `network` object.
#' 
#' @export
as_network <- function(x, ...) {
  UseMethod("as_network")
}

#' @rdname as_network
#' 
#' @export
#' 
as_network.igraph <- function(x, actor_type = TRUE) {
  vert_attr_names <- vrt_attr_names(x)
  if("vertex.names" %in% vert_attr_names){
    if("name" %in% vert_attr_names) {
      stop('\n`x` is an `igraph` object, but has vertex attributes "vertex.names".\n\n',
         'For `igraph` objects, the attribute name that refers to vertex names should be "name".\n\n',
         'For `network` objects, the attribute name that refers to vertex names should be "vertex.names".\n\n',
         'Since `x` has both, it is ambiguous which attribute should be used for vertex names.\n\n',
         'See `?igraph::set_vertex_attr` and `?igraph::delete_vertex_attr` to remove the ambiguity.',
         call. = FALSE)
    } else {
      warning('\n`x` is an `igraph` object, but has vertex attributes "vertex.names" AND "name".\n\n',
            'For `igraph` objects, the attribute name that refers to vertex names should be "name".\n\n',
            'For `network` objects, the attribute name that refers to vertex names should be "vertex.names".\n\n',
            'Since `x` does not contain a "name" vertex attribute, "vertex.names" is assumed to be intentional.\n\n',
            'See `?igraph::set_vertex_attr` and `?igraph::delete_vertex_attr` to remove the ambiguity and prevent this warning.',
            call. = FALSE)
    }
  }
  graph_attrs <- net_attrs(x)
  vert_attrs <- vrt_attrs(x)
  names(vert_attrs)[names(vert_attrs) == "name"] <- "vertex.names"
  edge_attrs <- edg_attrs(x)
  el <- rep_as_edgelist(x)
  
  if("type" %in% names(vert_attrs)) {
    if(actor_type) {
      bipartite_arg <- length(which(igraph::V(x)$type))
    } else {
      bipartite_arg <- length(which(!igraph::V(x)$type))
    }
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
as_network.default <- function(x, ...) {
  message("A method has not been implemented for `", class(x)[[1]], "` objects...\n",
          "\ttrying `network::as.network()`...\n")
    tryCatch({
        network::as.network(x, ...)
      },
      error = function(e) {
        stop("Objects of `", class(x)[[1]], "` are not supported.\n\n",
             "Use ?igraph::`igraph-package` or ?network::`network-package` to learn about supported objects.",
             call. = FALSE)
      })
}

#' @rdname as_network
#' 
#' @export
#' 
as_network.network <- function(x) {
  x
}

