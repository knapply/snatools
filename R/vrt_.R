#' Extract vertex attributes from a graph.
#' 
#' @param x An `igraph` or `network` object.
#' @param enforce_names
#' @param exclude_na
#' 
#' @return A named `list` of `x`'s vertex attributes.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#'
#' @export
vrt_attrs <- function(x, enforce_names = TRUE, exclude_na = NULL) {
  UseMethod("vrt_attrs")
}

#' @rdname vrt_attrs
#' 
#' @export
#' 
vrt_attrs.igraph <- function(ig, enforce_names = TRUE, exclude_na = NULL) {
  if(length(exclude_na)) {
    warning("Your `exclude_na` argument ignored. Not applicable to `igraph` objects.", 
            call. = FALSE)
  }
  out <- igraph::vertex_attr(ig)
  if(!"name" %in% names(out) && enforce_names) {
    out$name <- as.character(seq_len(igraph::vcount(ig)))
  }
  # names(out)[names(out) == "name"] <- "vertex.names"
  out
  # out[order(names(out))]
}

#' @rdname vrt_attrs
#' 
#' @export
#' 
vrt_attrs.network <- function(x, enforce_names = TRUE, exclude_na = TRUE) {
  out <- lapply(x$val, `[`)
  out <- do.call(rbind, out)
  out_names <- colnames(out)
  out <- lapply(seq_len(ncol(out)), function(x) unlist(out[, x]))
  names(out) <- out_names
  if(exclude_na) {
    out$na <- NULL
  }
  if(!"vertex.names" %in% names(out) && enforce_names) {
    out$vertex.names <- as.character(seq_len(network::network.size(x)))
  }
  # names(out)[names(out) == "vertex.names"] <- "name"
  out
  # out[order(names(out))]
}
 
#' List vertex attribute names.
#' 
#' @param x An `igraph` or `network` object.
#' @param exclude_na `network` objects include a `"na"` attribute indicating whether
#' a vertex is "missing" from the network. `exclude_na` determines whether or not to include
#' this `"na"` attribute in returned names. \cr
#' * Default: `TRUE`
#' * If `exclude_na` is provided when `x` is an `igraph` object, it is ignored with a `warning`.
#' 
#' @return `character` `vector`of `x`'s vertex attribute names.
#' 
#' @seealso [igraph::vertex_attr_names()], [network::list.vertex.attributes()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @return 
#' 
vrt_attr_names <- function(x, exclude_na = TRUE) {
  UseMethod("vrt_attr_names")
}

#' @rdname vrt_attr_names
#' 
#' @export
#' 
vrt_attr_names.igraph <- function(x, exclude_na = NULL) {
  if(length(exclude_na)) {
    warning("Your `exclude_na` argument ignored. Not applicable to `igraph` objects.", 
            call. = FALSE)
  }
  igraph::vertex_attr_names(x)
}

#' @rdname vrt_attr_names
#' 
#' @export
#' 
vrt_attr_names.network <- function(x, exclude_na = TRUE) {
  out <- unique(unlist(lapply(x$val, names)))
  if(!exclude_na) {
    return(out)
  }
  out[!out == "na"]
}


#' @export
#' 
vrt_attr <- function(x, vrt_attr) {
  UseMethod("vrt_attr")
}

#' @export
#' 
vrt_attr.igraph <- function(x, vrt_attr) {
  igraph::vertex_attr(x, vrt_attr)
}

#' @export
#' 
vrt_attr.network <- function(x, vrt_attr) {
  unlist(lapply(x$val, `[[`, vrt_attr))
}

#' @export
#' 
vrt_names <- function(x) {
  UseMethod("vrt_names")
}

#' @export
#' 
vrt_names.igraph <- function(x) {
  out <- vrt_attr(x, "name")
  if(!length(out)) {
    warning("Vertices do not have a 'name' attribute. Trying 'Name' instead.")
    out <- vrt_attr(x, "Name")
    if(!length(out)) {
      warning("Can't find `Name` either. Using vertex indices instead.")
      out <- seq_len(igraph::vcount(x))
    }
  }
  out
}

#' @export
#' 
vrt_names.network <- function(x) {
  out <- vrt_attr(x, "vertex.names")
  if(!length(out)) {
    warning("Vertices do not have a 'vertex.names' attribute. Trying 'name' instead.")
    out <- vrt_attr(x, "name")
    if(!length(out)) {
      warning("Can't find `name` either. Using vertex indices instead.")
      out <- seq_len(x$gal$n)
    }
  }
  out
}

