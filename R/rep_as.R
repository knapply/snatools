#' Build a networks's edge list representation.
#' 
#' `rep_as_edgelist()` creates edge lists in a _consistent_ format across classes.
#' 
#' @param x An `igraph` or `network` object.
#' @param use_names 
#' `logical` Whether the returned edge list should use vertex names instead of their
#'  indices. \cr
#'  Default: `FALSE`
#'
#' @details 
#' * If `x` is _directed_, columns are named `"from"` and `"to"` to indicate the
#' tie's direction.
#' * If `x` is _undirected_, columns are named `"vert1"` and `"vert2"`.
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
#'   rep_as_edgelist() %>% 
#'   head()
#'   
#' karate %>% 
#'   rep_as_edgelist(use_names = TRUE) %>% 
#'   head()
#' 
#' # network =============================================================================
#' data("sampson", package = "ergm") # loads "Sampson's Monks" as `samplike`
#' samplike
#' 
#' samplike %>% 
#'   rep_as_edgelist() %>% 
#'   head()
#'   
#' samplike %>% 
#'   rep_as_edgelist(use_names = TRUE) %>% 
#'   head()
#' 
#' @export
rep_as_edgelist <- function(x, use_names = FALSE) {
  UseMethod("rep_as_edgelist")
}

#' @rdname rep_as_edgelist
#' 
#' @export
#' 
rep_as_edgelist.igraph <- function(ig, use_names = FALSE) {
  out <- igraph::as_edgelist(ig, names = use_names)
  if(igraph::is_directed(ig)) {
    colnames(out) <- c("from", "to")
  } else {
    colnames(out) <- c("vert1", "vert2")
  }
  out
}

#' @rdname rep_as_edgelist
#' 
#' @export
#' 
rep_as_edgelist.network <- function(nw, use_names = FALSE) {
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

#' Build a network's adjacency matrix representation
#' 
#' @param x
#' @param edg_attr
#' @param sparse
#' @param ...
#' 
#' @return `matrix` or `dgCMatrix`
#' 
#' @export
#' 
rep_as_adjacency_matrix <- function(x, edg_attr = NULL, sparse = FALSE, ...) {
  UseMethod("rep_as_adjacency_matrix")
}

#' @rdname rep_as_adjacency_matrix
#' 
#' @export
#' 
rep_as_adjacency_matrix.igraph <- function(x, edg_attr = NULL, sparse = FALSE, ...) {
  igraph::as_adjacency_matrix(x, attr = edg_attr, sparse = sparse, ...)
}

#' @rdname rep_as_adjacency_matrix
#' 
#' @export
#' 
rep_as_adjacency_matrix.network <- function(x, edg_attr = NULL, sparse = FALSE) {
  out <- network::as.matrix.network.adjacency(x, attrname = edg_attr, ...)
  if(!sparse) {
    return(out)
  }
  Matrix::Matrix(out)
}

#' @export
rep_as_incidence_matrix <- function(x, edg_attr = NULL, sparse = FALSE, ...) {
  UseMethod("rep_as_incidence_matrix")
}

#' @rdname rep_as_incidence_matrix
#' 
#' @export
#' 
rep_as_incidence_matrix.igraph <- function(x, edg_attr = NULL, sparse = FALSE, ...) {
  igraph::as_incidence_matrix(attr = edg_attr, sparse = FALSE, ...)
}


#' @rdname rep_as_incidence_matrix
#' 
#' @export
#' 
rep_as_incidence_matrix.network <- function(x, edg_attr = NULL, sparse = FALSE, ...) {
  igraph::as_incidence_matrix(attr = edg_attr, sparse = FALSE, ...)
}

# nw <- 
# agilenet::southern_women %>%
#   # as_data_frame("vertices") %>% 
#   # dplyr::filter(type)
#   # igraph::set_vertex_attr("type", value = ifelse(V(.)$type, FALSE, TRUE)) %>%
#   # as_data_frame("vertices")
#   as_network(actor_type = TRUE)
# 
# nw %>% plot(label = "vertex.names")
# 
# nw %>% 
#   network::as.matrix.network.adjacency(expand.bipartite = FALSE)
