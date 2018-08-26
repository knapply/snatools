#' Edge Attributes
#' 
#' Extract edge attributes in a variety of formats.
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
#' @seealso 
#' [`igraph::edge_attr()`], [`igraph::edge_attr_names()`], [`igraph::as_data_frame()`],
#' [`network::get.edge.attribute()`], [`network::list.edge.attributes()`]
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
#' edg_get_attr_names(ig)
#' edg_get_attr_names(nw)
#' 
#' edg_get_attrs(ig) %>% lapply(head)
#' edg_get_attrs(nw)
#' 
#' edg_get_attrs_df(ig)
#' edg_get_attrs_df(nw)
#' 
#' edg_get_attr(ig, edg_attr = "Time") %>% head()
#' edg_get_attr(nw, edg_attr = "nominations")
#' 


#' @rdname extract-edge-attributes
#' 
#' @details 
#' * `edg_get_attrs` extract all edge attributes as a named `list`.
#' 
#' @export
#' 
edg_get_attrs <- function(x, ...) {
  UseMethod("edg_get_attrs")
}

#' @rdname extract-edge-attributes
#' 
#' @export
#' 
edg_get_attrs.igraph <- function(x) {
  out <- igraph::edge_attr(x)
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
#' * `edg_get_attrs_df` is a convenience wrapper around `edg_get_attrs()` that returns a
#' `data.frame`.
#'     + `stringsAsFactors` is _always_ `FALSE`.
#'     + If available, [`tibble::as_tibble()`] is called to return a `tbl_df` instead.
#' 
#' 
#' @export
edg_get_attrs_df <- function(x, try_tibble = TRUE, ...) {
  UseMethod("edg_get_attrs_df")
}

edg_get_attrs_df.igraph <- function(x, try_tibble = TRUE, ...) {
  out <- igraph::as_data_frame(x, what = "edges")
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
  }
  out
}

edg_get_attrs_df.network <- function(x, try_tibble = TRUE, ...) {
  el <- rep_edgelist(x, use_names = TRUE)
  attrs <- edg_get_attrs(x, ...)
  out <- cbind.data.frame(el, attrs, stringsAsFactors = FALSE)
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @details 
#' * `edg_get_attr` extracts the single edge attribute specified by `edg_attr` as a `vector`.
#' 
#' @export
#' 
edg_get_attr <- function(x, edg_attr, ...) {
  UseMethod("edg_get_attr")
}

#' @export
#' 
edg_get_attr.igraph <- function(x, edg_attr, ...) {
  out <- igraph::edge_attr(x, edg_attr, ...)
  if(!length(out)) {
    return(NULL)
  }
  out
}

#' @rdname extract-edge-attributes
#' 
#' @export
#' 
edg_get_attr.network <- function(x, edg_attr, ignore_na = TRUE, ...) {
  out <- network::get.edge.attribute(x$mel, attrname = edg_attr, na.omit = ignore_na, ...)
  if(!length(out)) {
    return(NULL)
  }
  out
}


#' @rdname extract-edge-attributes
#' 
#' @details 
#' * `edg_get_attr_names` extract the names of vertex attributes as a `character` `vector`.
#' 
#' @export
#' 
edg_get_attr_names <- function(x, ignore_na = TRUE) {
  UseMethod("edg_get_attr_names")
}

#' @rdname edg_get_attr_names
#' 
#' @export
#' 
edg_get_attr_names.igraph <- function(x) {
  out <- igraph::edge_attr_names(x)
  if(!length(out)) {
    return(NULL)
  }
  out
}

#' @rdname edg_get_attr_names
#' 
#' @export
#' 
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




