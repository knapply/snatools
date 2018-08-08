#' Extract vertex attributes from a graph.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A named `list` of `x`'s vertex attributes.
#'
#' @export
sna_get_vert_attrs <- function(x) {
  UseMethod("sna_get_vert_attrs")
}

#' @describeIn sna_get_vert_attrs
sna_get_vert_attrs.igraph <- function(ig) {
  out <- igraph::vertex_attr(ig)
  if(!"name" %in% names(out)) {
    out$name <- seq_len(igraph::vcount(ig))
  }
  names(out)[names(out) == "name"] <- "vertex.names"
  
  out[order(names(out))]
}


sna_get_vert_attrs.network <- function(nw) {
  out <- lapply(nw$val, `[`)
  out <- do.call(rbind, out)
  out <- apply(out, 2, as.list) 
  out <- lapply(out, unlist)
  out$na <- NULL
  names(out)[names(out) == "vertex.names"] <- "name"
  
  out[order(names(out))]
}
