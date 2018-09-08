#' Conversion to `igraph` objects.
#' 
#' @param x [`network::network`] object.
#' @param ... Additional arguments on to other methods (`clean_graph`, `actor_type`).
#' @param clean_graph logical. Whether to automatically call `clean_graph()` before converting x. \cr
#' Default: `TRUE`
#' @param actor_type logical. For bipartite graphs, the `"type"` (vertex attribute) that
#' "actor" vertices are to be assigned. \cr
#' Default: `TRUE`
#' 
#' @return An `igraph` ([`igraph::graph`]) object.
#' 
#' @export
#' 
as_igraph <- function(x, ...) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.igraph <- function(x) {
  x
}

#' @rdname as_igraph
#' 
#' @importFrom igraph graph_from_data_frame
#' @export 
as_igraph.network_dataset <- function(x) {
  graph_from_data_frame(d = x$edges, directed = x$directed, vertices = x$vertices)
}

#' @rdname as_igraph
#' 
#' @export
#' 
as_igraph.network <- function(x) {
  if (x$gal$hyper) {
    stop("Hypergraphs are not supported.", call. = FALSE)
  }
  net_attrs <- net_get_attrs(x, .default = NULL)
  
  vrt_attrs <- vrt_get_attrs(x)
  
  if (is.numeric(x$gal$bipartite)) {
    names(vrt_attrs) <- txt_replace(names(vrt_attrs), "is_actor", "type")
  }
  if ("vertex.names" %in% names(vrt_attrs)) {
    names(vrt_attrs)[names(vrt_attrs) == "vertex.names"] <- "name"
  }
  edg_attrs <- edg_get_attrs(x)
  vectorized_el <- as.vector(t(rep_edgelist(x)))
  
  out <- igraph::graph.empty(n = x$gal$n, directed = x$gal$directed)
  out <- igraph::add_edges(out, edges = vectorized_el)
  if (length(vrt_attrs)) {
    igraph::vertex_attr(out) <- vrt_attrs
    }
  if (length(net_attrs)) {
    igraph::graph_attr(out) <- net_attrs
    }
  if (length(edg_attrs)) {
    igraph::edge_attr(out) <- edg_attrs
    }
  out
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.tbl_graph <- function(x, ...) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop('The `tidygraph` package is required for this functionality. 
          Install it via `install.packages("tidygraph")`', call. = FALSE)
  }
  tidygraph::as.igraph(x, ...)
}

#' @rdname as_igraph
#' 
#' @export
#' 
as_igraph.ucinet <- function(x, ...) {
  graph_mode <- vapply(attr(x, "is_directed"), function(x) {
    switch(x, "TRUE" = "directed", "FALSE" = "undirected")
    }, character(1), USE.NAMES = FALSE)
  if (attr(x, "is_list")) {
    # TODO throws error if `weighted` is NULL, NA, or FALSE when `directed=TRUE`... 
    # TODO docs don't clarify, so resorting to explicit loops now
    is_weighted <- vapply(x, function(x) {
      any(!x %in% c(0, 1))
    }, logical(1))
    
    if (nrow(x[[1]]) == ncol(x[[1]])) {
        out <- vector("list", length = length(x))
        for (i in seq_along(x)) {
          if (is_weighted[[i]]) {
            out[[i]] <- igraph::graph_from_adjacency_matrix(adjmatrix = x[[i]],
                                                            mode = graph_mode[[i]],
                                                            weighted = is_weighted[[i]])
          } else {
            out[[i]] <- igraph::graph_from_adjacency_matrix(adjmatrix = x[[i]],
                                                            mode = graph_mode[[i]])
          }
        }
      names(out) <- names(x)
      return(out)
    }
    out <- vector("list", length = length(x))
    for (i in seq_along(x)) {
      if (is_weighted[[i]]) {
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
  if (nrow(x) == ncol(x)) {
    if (is_weighted) {
      return(igraph::graph_from_adjacency_matrix(adjmatrix = x, mode = graph_mode, 
                                                 weighted = is_weighted))
    }
    return(igraph::graph_from_adjacency_matrix(adjmatrix = x, mode = graph_mode))
  }
  if (is_weighted) {
    return(
      igraph::graph_from_incidence_matrix(incidence = x, weighted = is_weighted)
      )
  }
  igraph::graph_from_incidence_matrix(incidence = x)
}



