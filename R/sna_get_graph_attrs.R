#' Extract graph-level attributes from a graph.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A named `list` of `x`'s graph attributes.
#'
#' @export
sna_get_graph_attrs <- function(x) {
  UseMethod("sna_get_graph_attrs")
}

#' @rdname sna_get_graph_attrs
#' 
#' @export
#' 
sna_get_graph_attrs.igraph <- function(ig) {
  out <- igraph::graph_attr(ig)
  names(out)[names(out) == "vertex.names"] <- "name"
  
  out[order(names(out))]
}

#' @rdname sna_get_graph_attrs
#' 
#' @export
#' 
sna_get_graph_attrs.network <- function(nw) {
  attr_names <- names(nw$gal)
  out <- lapply(attr_names, function(x) nw$gal[[x]])
  names(out) <- attr_names
  junk_attrs <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
  out <- out[!names(out) %in% junk_attrs]
  
  if(length(unlist(out)) == 0L) {
    return(NULL)
  }
  out[order(names(out))]
}
