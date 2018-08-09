#' Build a graph's edge list representation.
#' 
#' `sna_get_edgelist()` creates edge lists in a consistent format across graph classes.
#' 
#' @param x An `igraph` or `network` object.
#' @param use_names 
#' `logical` Whether the returned edge list should use vertex names instead of their
#'  indices. \cr
#'  Default: `FALSE`
#'
#' @details 
#' * If `x` is a directed graph, columns are named `"from"` and `"to"` to indicate the
#' tie's direction.
#' * If `x` is an undirected graph, columns are named `"vert1"` and `"vert2"`.
#' 
#' @return `matrix` \cr
#' * `mode()`:
#'     + `numeric` if `use_names` is `FALSE`.
#'     + `character` if `use_names` is `TRUE`.
#' * `dim()`
#'     + `igraph::ecount(x)` by 2 if `x` is an `igraph` object.
#'     + `network::network.edgecount(x)` by 2 if `x` is a `network` object.
#' 
#' @seealso [`igraph::as_edgelist()`], [`network::as.edgelist()`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' # igraph ==============================================================================
#' data("karate", package = "igraphdata") # loads "Zachary's Karate Club" as `karate`
#' karate
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
#' data("sampson", package = "ergm") # loads "Sampson's Monks" as `samplike`
#' samplike
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
