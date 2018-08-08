#' Build an edge list of a graph
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A 2 column `matrix`. If graph is directed, the first column is always
#' the source of a tie.
#' 
#' @export
sna_as_edgelist <- function(x) {
  UseMethod("sna_as_edgelist")
}

#' @rdname sna_as_edgelist
#' 
sna_as_edgelist.igraph <- function(ig) {
  igraph::as_edgelist(ig, names = FALSE)
}

#' @rdname sna_as_edgelist
#' 
sna_as_edgelist.network <- function(nw) {
  if(!nw$gal$directed) {
    return(cbind(vapply(nw$mel, function(x) x[["inl"]], integer(1)),
            vapply(nw$mel, function(x) x[["outl"]], integer(1))))
    }
  cbind(vapply(nw$mel, function(x) x[["outl"]], integer(1)),
        vapply(nw$mel, function(x) x[["inl"]], integer(1))) 
}
