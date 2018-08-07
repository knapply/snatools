sna_get_edge_attrs <- function(x) {
  UseMethod("sna_get_edge_attrs")
}

sna_get_edge_attrs.igraph <- function(ig) {
  out <- igraph::edge_attr(ig)
  
  out[order(names(out))]
}

sna_get_edge_attrs.network <- function(nw) {
  out <- lapply(nw$mel, `[[`, "atl")
  out <- do.call(rbind, out)
  out <- apply(out, 2, as.list) 
  out <- lapply(out, unlist)
  out$na <- NULL
  
  out[order(names(out))]
}
