#' Metadata Queries
#' 
#' @rdname metadata-queries
#' 
#' @param x A graph object.
#' 
#' @examples
#' library(snatools)
#' 
#' data("karate", package = "igraphdata")
#' (ig <- karate)
#' 
#' data("emon", package = "network")
#' (nw <- emon$Cheyenne)
#' 
#' net_is_bipartite(ig)
#' net_is_bipartite(nw)
#' 
#' net_is_directed(ig)
#' net_is_directed(nw)

#' @rdname metadata-queries
#' 
#' @details `net_is_bipartite()` checks whether `x` is _labeled_ as bipartite and returns a `logical` scalar.
#'
#' @export
#' 
net_is_bipartite <- function(x) {
  UseMethod("net_is_bipartite")
}

#' @rdname metadata-queries
#' 
#' @export
#' 
net_is_bipartite.igraph <- function(x) {
  "type" %in% vrt_get_attr_names(x)
}

#' @rdname metadata-queries
#' 
#' @export
#' 
net_is_bipartite.network <- function(x) {
  is.numeric(x$gal$bipartite)
}

#' @rdname metadata-queries
#' 
#' @details `net_is_directed()` checks whether `x` is _labeled_ as directed and returns a `logical` scalar.
#'
#' @export
#' 
net_is_directed <- function(x) {
  UseMethod("net_is_directed")
}

#' @rdname metadata-queries
#'
#' @export
#' 
net_is_directed.igraph <- function(x) {
  igraph::is_directed(x)
}

#' @rdname metadata-queries
#'
#' @export
#' 
net_is_directed.network <- function(x) {
  isTRUE(x$gal$directed)
}