#' Extract graph attributes.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A named `list` of `x`'s graph attributes.
#'
#' @export
net_get_attrs <- function(x) {
  UseMethod("net_get_attrs")
}

#' @rdname net_get_attrs
#' 
#' @export
#' 
net_get_attrs.igraph <- function(ig) {
  out <- igraph::graph_attr(ig)
  names(out)[names(out) == "vertex.names"] <- "name"
  
  out[order(names(out))]
}

#' @rdname net_get_attrs
#' 
#' @export
#' 
net_get_attrs.network <- function(nw) {
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
