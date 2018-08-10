#' Convert graph objects to `igraph`.
#' 
#' @export
#' 
as_igraph <- function(x, ...) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' 
#' @export
#' 
as_igraph.network <- function(x) {
  if (x$gal$hyper) {
    stop("Hypergraphs are not supported.", call. = FALSE)
  }
  graph_attrs <- net_get_attrs(x)
  
  if(is.numeric(x$gal$bipartite)) {
    graph_attrs$loops <- NULL
    false_nodes <- seq_len(x$gal$bipartite)
    true_nodes <- seq.int(x$gal$bipartite + 1, x$gal$n)
    network::set.vertex.attribute(x, "type", value = FALSE, v = false_nodes)
    network::set.vertex.attribute(x, "type", value = TRUE, v = true_nodes)
  }
  vert_attrs <- vrt_get_attrs(x)
  edge_attrs <- edg_get_attrs(x)
  el <- rep_as_edgelist(x)
 
  out <- igraph::graph_from_edgelist(el, directed = x$gal$directed)
  igraph::graph_attr(out) <- graph_attrs
  igraph::edge_attr(out) <- edge_attrs
  igraph::vertex_attr(out) <- vert_attrs
  
  strictify(out)
}

#' @rdname as_igraph
#' 
#' @export
#' 
as_igraph.default <- function (x, ...) {
    tryCatch({
        igraph::as.igraph(x, ...)
      }, error = function(e) stop("Objects of ", class(x)[[1]],
        " are not supported at this time.", call. = FALSE))
}

#' @rdname as_igraph
#' 
#' @export
#' 
as_igraph.igraph <- function(x) {
  x
}

#' @rdname as_igraph
#' 
#' @export
#' 
as_igraph.ucinet <- function(x, ...) {
  graph_mode <- vapply(attr(x, "is_directed"), function(x) {
    switch(x, "TRUE" = "directed", "FALSE" = "undirected")
    }, character(1), USE.NAMES = FALSE)
  if(attr(x, "is_list")) {
    # TODO throws error if `weighted` is NULL, NA, or FALSE when `directed=TRUE`... 
    # TODO docs don't clarify, so resorting to explicit loops now
    is_weighted <- vapply(x, function(x) {
      any(!x %in% c(0, 1))
    }, logical(1))
    
    if(nrow(x[[1]]) == ncol(x[[1]])) {
        out <- vector("list", length = length(x))
        for(i in seq_along(x)) {
          if(is_weighted[[i]]) {
            out[[i]] <- igraph::graph_from_adjacency_matrix(adjmatrix = x[[i]],
                                                            mode = graph_mode[[i]],
                                                            weighted = is_weighted[[i]])
          } else {
            message("here")
            out[[i]] <- igraph::graph_from_adjacency_matrix(adjmatrix = x[[i]],
                                                            mode = graph_mode[[i]])
          }
        }
      names(out) <- names(x)
      return(out)
    }
    out <- vector("list", length = length(x))
    for(i in seq_along(x)) {
      if(is_weighted[[i]]) {
        out[[i]] <- igraph::graph_from_incidence_matrix(adjmatrix = x[[i]],
                                                        weighted = is_weighted[[i]])
        } else {
          out[[i]] <- igraph::graph_from_incidence_matrix(adjmatrix = x[[i]])
        }
      }
    names(out) <- names(x)
    return(out)
    }
  is_weighted <- any(!x %in% c(0, 1))
  if(nrow(x) == ncol(x)) {
    if(is_weighted) {
      return(igraph::graph_from_adjacency_matrix(adjmatrix = x, mode = graph_mode, 
                                                 weighted = is_weighted))
    }
    return(igraph::graph_from_adjacency_matrix(adjmatrix = x, mode = graph_mode))
  }
  if(is_weighted) {
    return(
      igraph::graph_from_incidence_matrix(incidence = x, weighted = is_weighted)
      )
  }
  igraph::graph_from_incidence_matrix(incidence = x, weighted = is_weighted)
}
