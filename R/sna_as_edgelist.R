#' Build an edge list of a graph
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A 2 column `matrix`. If graph is directed, the first column is always
#' the source of a tie.
#' 
#' @export
sna_as_edgelist <- function(x, use_names = FALSE) {
  UseMethod("sna_as_edgelist")
}

#' @rdname sna_as_edgelist
#' 
#' @export
#' 
sna_as_edgelist.igraph <- function(ig, use_names = FALSE) {
  igraph::as_edgelist(ig, names = use_names)
}

#' @rdname sna_as_edgelist
#' 
#' @export
#' 
sna_as_edgelist.network <- function(nw, use_names = FALSE) {
  outl <- lapply(nw$mel, `[[`, "outl")
  outl <- unlist(outl)
  inl <- lapply(nw$mel, `[[`, "inl")
  inl <- unlist(inl)
  if(nw$gal$directed) {
    out <- cbind(outl, inl)
  } else {
    out <- cbind(inl, outl)
  }
  if(!use_names) {
    return(out)
  }
  vert_names <- vapply(nw$val, function(x) x[["vertex.names"]], character(1))
  matrix(vert_names[out], ncol = 2)
}

# sna_as_edgelist.network <- function(nw) {
#   if(!nw$gal$directed) {
#     return(cbind(vapply(nw$mel, function(x) x[["inl"]], numeric(1)),
#                  vapply(nw$mel, function(x) x[["outl"]], numeric(1))))
#     }
#   cbind(vapply(nw$mel, function(x) x[["outl"]], numeric(1)),
#         vapply(nw$mel, function(x) x[["inl"]], numeric(1))) 
# }

# vrt_names <- vapply(nw$val, function(x) x[["vertex.names"]], character(1))
# 
# nw <- build_test_graph("nw")
# out <- cbind(vapply(nw$mel, function(x) x[["inl"]], numeric(1)),
#              vapply(nw$mel, function(x) x[["outl"]], numeric(1)))
# 
# matrix(vrt_names[out], ncol = 2) %>% head()
# vrt_names %>% head()
# nw %>% network::as.matrix.network.edgelist() %>% head()
