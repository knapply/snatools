#' Build an edge list of a graph
#' 
#' @param x An `igraph` or `network` object.
#' @param use_names `logical` Whether the returned edge list should use vertex names 
#'     instead of their indices. \cr
#'     Default: `FALSE`
#' 
#' @return A 2 column `matrix`. If graph is directed, the first column is always
#' the source of a tie.
#' 
#' @seealso [`igraph::as_edgelist()`], [`network::as.edgelist()`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' # igraph ==============================================================================
#' data("karate", package = "igraphdata")
#' 
#' karate %>% 
#'   sna_get_edgelist() %>% 
#'   head()
#'   
#' karate %>% 
#'   sna_get_edgelist(use_names = TRUE) %>% 
#'   head()
#' 
#' # network =============================================================================
#' data("sampson", package = "ergm")
#' 
#' samplike %>% 
#'   sna_get_edgelist() %>% 
#'   head()
#'   
#' samplike %>% 
#'   sna_get_edgelist(use_names = TRUE) %>% 
#'   head()
#' 
#' @export
sna_get_edgelist <- function(x, use_names = FALSE) {
  UseMethod("sna_get_edgelist")
}

#' @rdname sna_get_edgelist
#' 
#' @export
#' 
sna_get_edgelist.igraph <- function(ig, use_names = FALSE) {
  out <- igraph::as_edgelist(ig, names = use_names)
  if(igraph::is_directed(ig)) {
    colnames(out) <- c("from", "to")
  } else {
    colnames(out) <- c("vert1", "vert2")
  }
  out
}

#' @rdname sna_get_edgelist
#' 
#' @export
#' 
sna_get_edgelist.network <- function(nw, use_names = FALSE) {
  outl <- lapply(nw$mel, `[[`, "outl")
  outl <- unlist(outl)
  inl <- lapply(nw$mel, `[[`, "inl")
  inl <- unlist(inl)
  if(nw$gal$directed) {
    out <- cbind(outl, inl)
    colnames(out) <- c("from", "to")
  } else {
    out <- cbind(inl, outl)
    colnames(out) <- c("vert1", "vert2")
  }
  if(!use_names) {
    return(out)
  }
  vert_names <- vapply(nw$val, function(x) x[["vertex.names"]], character(1))
  matrix(vert_names[out], ncol = 2, dimnames = list(NULL, colnames(out)))
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
