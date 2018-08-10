#' Extract edge attributes.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A named `list` of `x`'s edge attributes.
#'
#' @export
edg_get_attrs <- function(x) {
  UseMethod("edg_get_attrs")
}

#' @rdname  edg_get_attrs
#' 
#' @export
#' 
edg_get_attrs.igraph <- function(ig) {
  out <- igraph::edge_attr(ig)
  
  out[order(names(out))]
}

#' @rdname edg_get_attrs
#' 
#' @export
#' 
edg_get_attrs.network <- function(nw) {
  out <- lapply(nw$mel, `[[`, "atl")
  out <- do.call(rbind, out)
  out <- apply(out, 2, as.list) 
  out <- lapply(out, unlist)
  out$na <- NULL
  
  out[order(names(out))]
}
