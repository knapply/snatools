#' @export
sna_as_edgelist <- function(x) {
  UseMethod("sna_as_edgelist")
}


sna_as_edgelist.igraph <- function(ig) {
  igraph::as_edgelist(ig, names = FALSE)
}


sna_as_edgelist.network <- function(nw) {
  if(!nw$gal$directed) {
    return(cbind(vapply(nw$mel, function(x) x[["inl"]], integer(1)),
            vapply(nw$mel, function(x) x[["outl"]], integer(1))))
    }
  cbind(vapply(nw$mel, function(x) x[["outl"]], integer(1)),
        vapply(nw$mel, function(x) x[["inl"]], integer(1))) 
}
