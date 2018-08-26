#' Extract Vertex Attributes
#' 
#' @rdname extract-vertex-attributes
#' 
#' @param x A graph object.
#' @param ... Arguments passed on to other methods. See below.
#' @param ignore_na `logical`. Whether to ignore the `"na"` attribute of 
#' [`network::network`] objects. \cr
#' Default: `TRUE`
#' @param vrt_attr `character`. Name of target attribute.
#' @param try_tibble `logical`. 
#' Whether to attempt calling [`tibble::as_tibble()`] in `vrt_get_attrs_df()`. \cr
#' Default: `TRUE`
#' 
#' @return A named `list`, `vector`, `data.frame`, or [`tibble::tibble`]. See Details.
#' 
#' @seealso 
#' [`igraph::vertex_attr()`], [`igraph::vertex_attr_names()`], [`igraph::as_data_frame()`],
#' [`network::get.vertex.attribute()`], [`network::list.vertex.attributes()`]
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
#' vrt_get_attr_names(ig)
#' vrt_get_attr_names(nw)
#' 
#' vrt_get_attrs(ig)
#' vrt_get_attrs(nw)
#' 
#' vrt_get_attrs_df(ig)
#' vrt_get_attrs_df(nw)
#' 
#' vrt_get_attr(ig, vrt_attr = "Faction")
#' vrt_get_attr(nw, vrt_attr = "Sponsorship")
#' 
#' vrt_get_names(ig)
#' vrt_get_names(nw)


#' @rdname extract-vertex-attributes 
#' 
#' @details 
#' * `vrt_get_attrs` extracts all vertex attributes as a named `list`.
#' 
#' @export
#'
vrt_get_attrs <- function(x, ...) {
  UseMethod("vrt_get_attrs")
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_attrs.igraph <- function(x) {
  out <- igraph::vertex_attr(x)
  if(!length(out)) {
    out <- NULL
  }
  out
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_attrs.network <- function(x, ignore_na = TRUE) {
  out <- lapply(x$val, `[`)
  out <- do.call(rbind, out)
  out_names <- colnames(out)
  out <- lapply(seq_len(ncol(out)), function(x) unlist(out[, x]))
  names(out) <- out_names
  if(ignore_na) {
    out$na <- NULL
  }
  out
}
 
#' @rdname extract-vertex-attributes
#' 
#' @details 
#' * `vrt_get_attrs_df` is a convenience wrapper around `vrt_get_attrs()` that returns a
#' `data.frame`.
#'     + `stringsAsFactors` is _always_ `FALSE`.
#'     + If available, [`tibble::as_tibble()`] is called to return a `tbl_df` instead.
#' 
#' @export
vrt_get_attrs_df <- function(x, ...) {
  UseMethod("vrt_get_attrs_df")
}

#' @export
#' 
vrt_get_attrs_df.igraph <- function(x, try_tibble = TRUE, ...) {
  out <- igraph::as_data_frame(x, what = "vertices")
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
  }
  out
}

#' @export
#' 
vrt_get_attrs_df.network <- function(x, try_tibble = TRUE, ...) {
  out <- vrt_get_attrs(x, ...)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if(try_tibble) {
    if(requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
  }
  out
}

#' @rdname extract-vertex-attributes
#' 
#' @details 
#' * `vrt_get_attr` extracts the single vertex attribute specified by `vrt_attr` as a `vector`.
#' 
#' @export
#' 
vrt_get_attr <- function(x, vrt_attr) {
  UseMethod("vrt_get_attr")
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_attr.igraph <- function(x, vrt_attr) {
  if(!vrt_attr %in% vrt_get_attr_names(x)) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  igraph::vertex_attr(x, vrt_attr)
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_attr.network <- function(x, vrt_attr) {
  if(!vrt_attr %in% vrt_get_attr_names(x)) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  unlist(lapply(x$val, `[[`, vrt_attr))
}


#' @rdname extract-vertex-attributes
#' 
#' @details 
#' * `vrt_get_names` is a convenience wrapper around `vrt_get_attr` to extract the names
#' of vertices as a `vector`. 
#'     + It assumes `igraph` objects use `"name"` and `network` objects use 
#'      `"vertex.names"`. 
#'     + If a vertex attribute following this convention is not present, `vrt_get_names()`
#'       throws a warning and returns `NULL`.
#' 
#' @export
#' 
vrt_get_names <- function(x) {
  UseMethod("vrt_get_names")
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_names.igraph <- function(x) {
  out <- vrt_get_attr(x, "name")
  if(!length(out)) {
    warning("`x` has no attribute called 'vertex.names'. Returning `NULL`")
    out <- NULL
  }
  out
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_names.network <- function(x) {
  out <- vrt_get_attr(x, "vertex.names")
  if(!length(out)) {
    warning("`x` has no attribute called 'vertex.names'. Returning `NULL`")
    out <- NULL
  }
  out
}


#' @rdname extract-vertex-attributes
#' 
#' @details 
#' * `vrt_get_attr_names` extract the names of vertex attributes as a `character` `vector`.
#' 
#' @export
#' 
vrt_get_attr_names <- function(x, ...) {
  UseMethod("vrt_get_attr_names")
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_attr_names.igraph <- function(x) {
  out <- igraph::vertex_attr_names(x)
  if(!length(out)) {
    out <- NULL
  }
  out
}

#' @rdname extract-vertex-attributes
#' 
#' @export
#' 
vrt_get_attr_names.network <- function(x, ignore_na = TRUE) {
  out <- unique(unlist(lapply(x$val, names)))
  if(!length(out)) {
    return(NULL)
  }
  if(!ignore_na) {
    return(out)
  }
  out[!out == "na"]
}
