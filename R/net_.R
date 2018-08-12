#' Extract graph attributes.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A named `list` of `x`'s graph attributes.
#'
#' @export
net_attrs <- function(x) {
  UseMethod("net_attrs")
}

#' @rdname net_attrs
#' 
#' @export
#' 
net_attrs.igraph <- function(x) {
  # out <- igraph::graph_attr(ig)
  igraph::graph_attr(x)
  # names(out)[names(out) == "vertex.names"] <- "name"
  
  # out[order(names(out))]
}

#' @rdname net_attrs
#' 
#' @export
#' 
net_attrs.network <- function(nw) {
  attr_names <- names(nw$gal)
  out <- lapply(attr_names, function(x) nw$gal[[x]])
  names(out) <- attr_names
  metadata <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
  out <- out[!names(out) %in% metadata]
  if(length(unlist(out)) == 0L) {
    return(NULL)
  }
  out
  # out[order(names(out))]
}

# net_attrs.network <- function(x) {
#   attr_names <- net_attr_names(x)
#   if(length(attr_names) > 1L){
#     out <- lapply(attr_names, function(x) x$gal[[x]])
#   } else {
#     out <- x$gal[[attr_names]]
#   }
#   names(out) <- attr_names
#   metadata <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
#   out <- out[!names(out) %in% metadata]
#   if(length(unlist(out)) == 0L) {
#     return(NULL)
#   }
#   out
# }


#' List a network/graph's attribute names.
#' 
#' @param x An `igraph` or `network` object.
#' @param exclude_metadata `logical` objects include what can be described as
#' "metadata" in their network/graph-level attributes. 
#' * Default: `TRUE`
#' These metadata include:
#' * `"n"`: The number of vertices in the `network` object.
#' * `"directed"`: Whether the `network` object is directed.
#' * `"hyper"`: Whether the `network` object is a hypergraph.
#' * `"multiple"`: Whether the `network` object is multiplex.
#' * `"bipartite"`: Whether the `network` object is bipartite.
#' * `"mnext"`: The index of the next edge to be added to the `network` object.
#' `exclude_metadata` determines whether or not to include these in the returned names.
#' * If `exclude_metadata` is provided when `x` is an `igraph` object, it is ignored with 
#' a `warning`.
#' 
#' @return `character` `vector`of `x`'s network/graph attribute names.
#' 
#' @seealso [igraph::edge_attr_names()], [network::list.edge.attributes()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
#' 
net_attr_names <- function(x, exclude_metadata = TRUE) {
  UseMethod("net_attr_names")
}

#' @rdname net_attr_names
#' 
#' @export
#' 
net_attr_names.igraph <- function(x, exclude_metadata = NULL) {
  if(length(exclude_metadata)) {
    warning("`exclude_metadata` argument ignored. Not applicable to `igraph` objects.", 
            call. = FALSE)
  }
  igraph::graph_attr_names(x)
}

#' @rdname net_attr_names
#' 
#' @export
#' 
net_attr_names.network <- function(x, exclude_metadata = TRUE) {
  out <- names(x$gal)
  if(!exclude_metadata) {
    return(out)
  }
  metadata <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
  out[!out %in% metadata]
}
