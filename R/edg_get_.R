#' Extract Edge Attributes
#' 
#' Extract edge attributes in a variety of formats consistent across
#' `igraph` and `network` objects.
#' 
#' @rdname extract-edge-attributes
#' 
#' @param x A graph object.
#' @param ... Arguments passed on to other methods. See below.
#' @param ignore_na `logical`. Whether to ignore the `"na"` attribute of 
#' [`network::network`] objects. \cr
#' Default: `TRUE`
#' @param edg_attr `character`. Name of target attribute.
#' @param try_tibble `logical`. 
#' Whether to attempt calling [`tibble::as_tibble()`] in `edg_get_attrs_df()`. \cr
#' Default: `TRUE`
#' 
#' @return A named `list`, `vector`, `data.frame`, or [`tibble::tibble`]. See Details.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples
#' library(snatools)
#' 
#' data("enron", package = "igraphdata")
#' (ig <- enron)
#' 
#' data("sampson", package = "ergm")
#' (nw <- samplike)
#' 


#' @rdname extract-edge-attributes
#' 
#' @details 
#' * `edg_get_attrs` extract all edge attributes as a named `list`.
#' 
#' @seealso 
#' [`igraph::edge_attr()`]
#' 
#' @examples 
#' edg_get_attrs(ig) %>% lapply(head)
#' edg_get_attrs(nw)
#' 
#' @export
edg_get_attrs <- function(x, ...) {
  UseMethod("edg_get_attrs")
}

#' @rdname extract-edge-attributes
#' 
#' @importFrom igraph edge_attr
#' @export
edg_get_attrs.igraph <- function(x) {
  out <- edge_attr(x)
  if(!length(out)) {
    return(NULL)
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @export
#' 
edg_get_attrs.network <- function(x, ignore_na = TRUE) {
  out <- lapply(x$mel, `[[`, "atl")
  out <- do.call(rbind, out)
  out <- apply(out, 2, as.list) 
  out <- lapply(out, unlist)
  if(ignore_na) {
    out$na <- NULL
  }
  if(!length(out)) {
    return(NULL)
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @details 
#' * `edg_get_attrs_df()` is a convenience wrapper around `edg_get_attrs()` that returns a
#' `data.frame`.
#'     + `stringsAsFactors` is _always_ `FALSE`.
#'     + If `try_tibble` is `TRUE` and the `tibble` package is available, a 
#'     [`tibble::tibble`] is returned.
#' 
#' @seealso 
#' [`igraph::as_data_frame()`]
#' 
#' @examples
#' edg_get_attrs_df(ig)
#' edg_get_attrs_df(nw)
#' 
#' @export
edg_get_attrs_df <- function(x, ...) {
  UseMethod("edg_get_attrs_df")
}

#' @rdname extract-edge-attributes
#' 
#' @importFrom igraph as_data_frame
#' @importFrom tibble as_tibble
#' @export
edg_get_attrs_df.igraph <- function(x, try_tibble = TRUE, ...) {
  out <- as_data_frame(x, what = "edges")
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- as_tibble(out)
    }
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @importFrom tibble as_tibble
#' @export
edg_get_attrs_df.network <- function(x, try_tibble = TRUE, ...) {
  el <- rep_edgelist(x, use_names = TRUE)
  attrs <- edg_get_attrs(x, ...)
  out <- cbind.data.frame(el, attrs, stringsAsFactors = FALSE)
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- as_tibble(out)
    }
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @details 
#' * `edg_get_attr()` extracts the single edge attribute specified by `edg_attr` as a `vector`.
#' 
#' @seealso 
#' [`igraph::edge_attr()`], [`network::get.edge.attribute()`]
#' 
#' @examples 
#' edg_get_attr(ig, "Time)
#' edg_get_attr(nw, "nominations")
#' 
#' @export
edg_get_attr <- function(x, edg_attr, ...) {
  UseMethod("edg_get_attr")
}

#' @rdname extract-edge-attributes
#' 
#' @importFrom igraph edge_attr
#' @export
edg_get_attr.igraph <- function(x, edg_attr, ...) {
  out <- edge_attr(x, edg_attr, ...)
  if(!length(out)) {
    return(NULL)
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @importFrom network get.edge.attribute
#' @export
edg_get_attr.network <- function(x, edg_attr, ignore_na = TRUE, ...) {
  out <- get.edge.attribute(x$mel, attrname = edg_attr, na.omit = ignore_na, ...)
  if(!length(out)) {
    return(NULL)
  }
  out
}


#' @rdname extract-edge-attributes
#' 
#' @details 
#' * `edg_get_attr_names()` extract the names of vertex attributes as a `character` `vector`.
#'   + `edg_get_attr_names()` differs from `network::list.edge.attributes()`, which
#'   sorts attribute names alphabetically.
#' 
#' @seealso 
#' [`igraph::edge_attr_names()`], [`network::list.edge.attributes()`]
#' 
#' @export
edg_get_attr_names <- function(x, ...) {
  UseMethod("edg_get_attr_names")
}

#' @rdname extract-edge-attributes
#' 
#' @importFrom igraph edge_attr_names
#' @export
edg_get_attr_names.igraph <- function(x) {
  out <- edge_attr_names(x)
  if(!length(out)) {
    return(NULL)
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @export
edg_get_attr_names.network <- function(x, ignore_na = TRUE) {
  out <- lapply(x$mel, `[[`, "atl")
  out <- unique(unlist(lapply(out, names)))
  if(!length(out)) {
    return(NULL)
  }
  if(!ignore_na) {
    return(out)
  }
  out[!out == "na"]
}
