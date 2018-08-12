#' Extract edge attributes.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A named `list` of `x`'s edge attributes.
#'
#' @export
edg_attrs <- function(x, exclude_na = TRUE) {
  UseMethod("edg_attrs")
}

#' @rdname  edg_attrs
#' 
#' @export
#' 
edg_attrs.igraph <- function(x, exclude_na = NULL) {
  if(length(exclude_na)) {
    warning("Your `exclude_na` argument was ignored. Not applicable to `igraph` objects.", 
            call. = FALSE)
  }
  igraph::edge_attr(x)
}

#' @rdname edg_attrs
#' 
#' @export
#' 
edg_attrs.network <- function(x, exclude_na = TRUE) {
  out <- lapply(x$mel, `[[`, "atl")
  out <- do.call(rbind, out)
  out <- apply(out, 2, as.list) 
  out <- lapply(out, unlist)
  if(exclude_na) {
    out$na <- NULL
  }
  out
}


#' List edge attribute names.
#' 
#' @param x An `igraph` or `network` object.
#' @param exclude_na `network` objects include a `"na"` attribute indicating whether
#' a vertex is "missing" from the network. `exclude_na` determines whether or not to include
#' this `"na"` attribute in returned names. \cr
#' * Default: `TRUE`
#' * If `exclude_na` is provided when `x` is an `igraph` object, it is ignored with a `warning`.
#' 
#' @return `character` `vector`of `x`'s edge attribute names.
#' 
#' @seealso [igraph::edge_attr_names()], [network::list.edge.attributes()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
#' 
edg_attr_names <- function(x, exclude_na = TRUE) {
  UseMethod("edg_attr_names")
}

#' @rdname edg_attr_names
#' 
#' @export
#' 
edg_attr_names.igraph <- function(x, exclude_na = NULL) {
  if(length(exclude_na)) {
    warning("Your `exclude_na` argument was ignored. Not applicable to `igraph` objects.", 
            call. = FALSE)
  }
  igraph::edge_attr_names(x)
}

#' @rdname edg_attr_names
#' 
#' @export
#' 
edg_attr_names.network <- function(x, exclude_na = TRUE) {
  out <- lapply(nw$mel, `[[`, "atl")
  out <- unique(unlist(lapply(out, names)))
  if(!exclude_na) {
    return(out)
  }
  out[!out == "na"]
}
