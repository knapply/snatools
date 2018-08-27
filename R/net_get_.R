#' Extract Global Network Attributes
#' 
#' Extract global network attributes in a variety of formats consistent across
#' `igraph` and `network` objects.
#' 
#' @rdname extract-network-attributes
#' 
#' @param x An `igraph` ([`igraph::graph`]) or `network::network` object.
#' @param ... Arguments passed on to other methods. See other arguments.
#' @param net_attr `character`. Name of target attribute.
#' @param drop_metadata `logical`. Whether or not to drop a [`network::network`]'s 
#' metadata attributes from returned object. See Details. \cr
#' Default: `TRUE`
#' @param try_tibble `logical`. 
#' Whether to attempt calling [`tibble::as_tibble()`] in `vrt_get_attrs_df()`. \cr
#' Default: `TRUE`
#' @param .default If returned value is empty (i.e. `x` has no global network attributes
#' or none that match the target specified by `net_attr`), `.default` is returned instead. \cr
#' Default: `NA`
#' 
#' @details 
#' * `drop_metadata`
#'   + [`network::network`] objects automatically include what can be described as metadata in their 
#' global network attributes. These metadata are:
#'     + `"n"` (`numeric`): The number of vertices in the `network` object.
#'     + `"directed"` (`logical`): Whether the `network` object's edges are meant to be interpreted as directed.
#'     + `"hyper"` (`logical`): Whether the `network` object is a hypergraph.
#'     + `"multiple"` (`logical`): Whether the `network` object's edges are meant to be interpreted as multiplex.
#'     + `"bipartite"` (`logical` _or_ `numeric`): `FALSE`, if the `network` object is not bipartite _or_ 
#'   the number of vertices belonging to the its "actors" mode if it is bipartite.
#'     + `"mnext"` (`numeric`): The index of the next edge to be added to the `network` object.
#'   + `drop_metadata` decides whether to include these in the returned object.
#' 
#' @return A named `list`, `vector`, `data.frame`, or [`tibble::tibble`]. See Details.
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
#' @export


#' @rdname extract-network-attributes
#' 
#' @details 
#' * `net_get_attrs()` extracts all global network attributes as a named `list`.
#'
#' @examples 
#' net_get_attrs(ig)
#' net_get_attrs(nw, drop_metadata = FALSE)
#'
#' @export
net_get_attrs <- function(x, ...) {
  UseMethod("net_get_attrs")
}

#' @rdname extract-network-attributes
#' 
#' @importFrom igraph graph_attr
#' @export
net_get_attrs.igraph <- function(x, .default = NA) {
  out <- graph_attr(x)
  if(!length(unlist(out))) {
    return(.default)
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @export
net_get_attrs.network <- function(x, drop_metadata = TRUE, .default = NA) {
  attr_names <- names(x$gal)
  out <- lapply(attr_names, function(y) x$gal[[y]])
  names(out) <- attr_names
  if(drop_metadata) {
    metadata <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
    out <- out[!names(out) %in% metadata]
  }
  if(!length(unlist(out))) {
    return(.default)
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @details 
#' * `net_get_attrs_df()` is a convenience wrapper around `net_get_attrs()` that returns a
#' `data.frame`.
#'     + `stringsAsFactors` is _always_ `FALSE`.
#'     + If `try_tibble` is `TRUE` and the `tibble` package is available, a 
#'     [`tibble::tibble`] is returned.
#'
#' @examples
#' net_get_attrs_df(ig)
#' net_get_attrs_df(nw, drop_metadata = FALSE)
#' 
#' @importFrom tibble as_tibble
#' @export
net_get_attrs_df <- function(x, try_tibble = TRUE, ...) {
  out <- net_get_attrs(x, ...)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- as_tibble(out)
    }
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @details 
#' * `net_get_attr()` extracts the single global, network-level attribute specified 
#' by `net_attr` and returns a scalar.
#'
#' @examples 
#' net_get_attr(ig, "Author")
#'
#' @export
net_get_attr <- function(x, net_attr) {
  UseMethod("net_get_attr")
}

#' @rdname extract-network-attributes
#' 
#' @importFrom igraph graph_attr
#' @export
net_get_attr.igraph <- function(x, net_attr, .default = NA) {
  out <- graph_attr(x, net_attr)
  if(!length(unlist(out))) {
    return(.default)
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @export
net_get_attr.network <- function(x, net_attr, .default = NA) {
  out <- x$gal[[net_attr]]
  if(!length(unlist(out))) {
    return(.default)
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @details 
#' * `net_get_attr_names()` extracts the names of network/graph-level attributes as
#' a `character` `vector`.
#'
#' @examples
#' net_get_attr_names(ig)
#' net_get_attr_names(nw, drop_metadata = FALSE)
#'
#' @export
#' 
net_get_attr_names <- function(x, ...) {
  UseMethod("net_get_attr_names")
}

#' @rdname extract-network-attributes
#' 
#' @importFrom igraph graph_attr_names
#' @export
net_get_attr_names.igraph <- function(x, .default = NA) {
  out <- graph_attr_names(x)
  if(!length(unlist(out))) {
    return(.default)
  }
  out
}

#' @rdname extract-network-attributes
#' 
#' @export
net_get_attr_names.network <- function(x, drop_metadata = TRUE, .default = NA) {
  out <- names(x$gal)
  if(!drop_metadata) {
    return(out)
  }
  metadata <- c("n", "directed", "hyper", "multiple", "bipartite", "mnext")
  out <- out[!out %in% metadata]
  if(!length(unlist(out))) {
    return(.default)
  }
  out
}
