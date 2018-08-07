sna_as_edgelist <- function(x) {
  UseMethod("sna_as_edgelist")
}

sna_as_edgelist.igraph <- function(ig) {
  igraph::as_edgelist(ig, names = FALSE)
}

# sna_as_edgelist.network <- function(nw) {
#   el <- network::as.matrix.network.edgelist(nw)
#   vert_names <- attr(el, "vnames")
#   names(vert_names) <- seq_along(vert_names)
#   # el <- matrix(as.character(el), ncol = 2)
#   el
#   # matrix(vert_names[el], ncol = 2)
# }

sna_as_edgelist.network <- function(nw) {
  if(!nw$gal$directed) {
    return(cbind(vapply(nw$mel, function(x) x[["inl"]], integer(1)),
            vapply(nw$mel, function(x) x[["outl"]], integer(1))))
    }
  cbind(vapply(nw$mel, function(x) x[["outl"]], integer(1)),
        vapply(nw$mel, function(x) x[["inl"]], integer(1))) 
}

# cbind(
#   vapply(nw$mel, function(x) x[["outl"]], integer(1)),
#   vapply(nw$mel, function(x) x[["inl"]], integer(1))
# ) %>% head()
# 
# network::as.matrix.network.edgelist(nw) %>% head()
# 
# cbind(unlist(sapply(x$mel, "[[", "outl")), unlist(sapply(x$mel, 
#         "[[", "inl")))




