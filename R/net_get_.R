#' Extract Graph-Level Attributes
#' 
#' @rdname extract-network-attributes
#' 
#' @param x A graph object.
#' @param ... Arguments passed on to other methods. See below.
#' @param drop_metadata `logical`. Whether or not to drop metadata attributes from
#' returned object. See Details. \cr
#' Default: `FALSE`
#' [`network::network`] objects include what can be 
#' described as "metadata" in their graph-level attributes.
#' @param try_tibble `logical`. 
#' Whether to attempt calling [`tibble::as_tibble()`] in `vrt_get_attrs_df()`. \cr
#' Default: `TRUE`
#' @param net_attr `character`. Name of target attribute.
#' 
#' @details 
#' `drop_metadata`
#' * [`network::network`] object include what can be described as metadata in their 
#' network/graph-level attributes. These metadata include: \cr
#'   +`"n"`: The number of vertices in the `network` object.
#'   + `"directed"`: Whether the `network` object is directed.
#'   + `"hyper"`: Whether the `network` object is a hypergraph.
#'   + `"multiple"`: Whether the `network` object is multiplex.
#'   + `"bipartite"`: Whether the `network` object is bipartite.
#'   + `"mnext"`: The index of the next edge to be added to the `network` object. \cr
#' * `drop_metadata` determines whether or not to include these in the returned object.
#' 
#' @return A named `list`, `vector`, `data.frame`, or [`tibble::tibble`]. See Details.
#' 
#' @seealso 
#' [`igraph::graph_attr()`], [`igraph::graph_attr_names()`], 
#' [`network::get.network.attribute()`], [`network::list.network.attributes()`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
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
#' net_get_attr_names(ig)
#' net_get_attr_names(nw, drop_metadata = FALSE)
#' 
#' net_get_attrs(ig)
#' net_get_attrs(nw, drop_metadata = FALSE)
#' 
#' net_get_attrs_df(ig)
#' net_get_attrs_df(nw, drop_metadata = FALSE)
#' 
#' net_get_attr(ig, "Author")
#'

#' @rdname extract-network-attributes
#' 
#' @details `net_get_attrs()` extracts all vertex attributes as a named list.
#'
#' @export
net_get_attrs <- function(x, ...) {
  UseMethod("net_get_attrs")
}

#' @rdname extract-network-attributes
#' 
#' @export
#' 
net_get_attrs.igraph <- function(x) {
  igraph::graph_attr(x)
}

#' @rdname net_get_attrs
#' 
#' @export
#' 
net_get_attrs.network <- function(x, drop_metadata = TRUE) {
  attr_names <- names(x$gal)
  out <- lapply(attr_names, function(y) x$gal[[y]])
  names(out) <- attr_names
  if(drop_metadata) {
    metadata <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
    out <- out[!names(out) %in% metadata]
  }
  if(length(unlist(out)) == 0L) {
    return(NULL)
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @details 
#' * `net_get_attrs_df` is a convenience wrapper around `net_get_attrs()` that returns a
#' `data.frame`.
#'     + `stringsAsFactors` is _always_ `FALSE`.
#'     + If available, [`tibble::as_tibble()`] is called to return a `tbl_df` instead.
#' 
#' @export
net_get_attrs_df <- function(x, try_tibble = TRUE, ...) {
  out <- net_get_attrs(x, ...)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @details `net_get_attr()` extracts the single network/graph-level attribute specified 
#' by `net_attr` and returns a scalar.
#'
#' @export
#' 
net_get_attr <- function(x, net_attr) {
  UseMethod("net_get_attr")
}

net_get_attr.igraph <- function(x, net_attr) {
  out <- igraph::graph_attr(x, net_attr)
  if(!length(out)) {
    warning("`x` has no attribute with that name. Returning `NULL`.")
    return(NULL)
  }
  out
}

net_get_attr.network <- function(x, net_attr) {
  out <- x$gal[[net_attr]]
  if(!length(out)) {
    warning("`x` has no attribute with that name. Returning `NULL`.")
    return(NULL)
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @details `net_get_attr_names()` extracts the names of network/graph-level attributes as
#' a `character` `vector`.
#'
#' @export
#' 
net_get_attr_names <- function(x, ...) {
  UseMethod("net_get_attr_names")
}

#' @rdname extract-network-attributes
#' 
#' @export
#' 
net_get_attr_names.igraph <- function(x) {
  igraph::graph_attr_names(x)
}

#' @rdname extract-network-attributes
#' 
#' @export
#' 
net_get_attr_names.network <- function(x, drop_metadata = TRUE) {
  out <- names(x$gal)
  if(!drop_metadata) {
    return(out)
  }
  metadata <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
  out[!out %in% metadata]
}
