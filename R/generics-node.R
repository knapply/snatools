#' Get a Sequence of Nodes for Indexing
#'
#' @template param-x
#'
#' @return `<integer>`
#'
#' @template author-bk
#'
#' @seealso [igraph::V()]
#'
#' @examples
#' ig <- example_igraph()
#' seq_nodes(ig)
#'
#' nw <- example_network()
#' seq_nodes(nw)
#'
#' @export
seq_nodes <- function(x) {
  seq_len(node_count(x))
}


#' How Many Nodes Are There?
#'
#' @template param-x
#'
#' @return `<integer>`
#'
#' @template author-bk
#'
#' @examples
#' ig <- example_igraph()
#' node_count(ig)
#'
#' nw <- example_network()
#' node_count(nw)
#'
#' @export
node_count <- function(x) {
  UseMethod("node_count")
}

#' @rdname node_count
#' @importFrom igraph vcount
#' @export
node_count.igraph <- function(x) {
  vcount(x)
}

#' @rdname node_count
#' @export
node_count.network <- function(x) {
  out <- x[["gal"]][["n"]]
  if (is.null(out)) {
    stop("`x` is malformed:\n\t- `x$gal$n` is missing", call. = FALSE)
  }
  out
}


#' Get Node Attributes
#'
#' @param x `<igraph>` or `<network>`
#' @param attr_name `<character>` (scalar)
#' @param which_nodes `<numeric>` or `<logical>`
#' @template param-dots
#'
#' @return `atomic` `vector` or `<list>`
#'
#' @examples
#' ig <- example_igraph()
#' node_count(ig)
#'
#' nw <- example_network()
#' node_count(nw)
#'
#' @export
node_get_attr <- function(x, attr_name, which_nodes = seq_nodes(x), ...) {
  UseMethod("node_get_attr")
}

#' @rdname node_get_attr
#'
#' @importFrom igraph vertex_attr
#'
#' @export
node_get_attr.igraph <- function(x, attr_name, which_nodes = seq_nodes(x), ...) {
  vertex_attr(graph = x, name = attr_name, index = which_nodes)
}

#' @rdname node_get_attr
#'
#' @param auto_unlist If possible, safely flatten the result to an atomic vector.
#'
#' @export
node_get_attr.network <- function(x, attr_name, which_nodes = seq_nodes(x),
                                  auto_unlist = TRUE, ...) {
  if (!is.logical(auto_unlist)) {
    stop("`auto_unlist` must be either `TRUE` or `FALSE`.", call. = FALSE)
  }

  if (is.character(which_nodes)) {
    which_nodes <- node_get_names(x) %in% which_nodes
  }

  out <- lapply(x[["val"]], `[[`, attr_name)[which_nodes]

  if (!auto_unlist) {
    return(out)
  }

  attr_lengths <- .nap_int(out, length, nm = FALSE)
  if (any(attr_lengths > 1L)) { # non-scalars present
    return(out)
  }

  any_recursive <- any(.nap_lgl(out, is.recursive, nm = FALSE))
  if (any_recursive) {
    return(out)
  }

  types <- unique(.nap_chr(out, typeof, nm = FALSE))
  if (length(types) > 1L) { # values are heterogeneous
    return(out)
  }

  all_flat <- all(.nap_lgl(out, function(.x) is.null(dim(.x))))
  if (!all_flat) { # leave matrices/arrays intact
    return(out)
  }

  out[attr_lengths == 0L] <- `storage.mode<-`(NA, types[[1L]])

  unlist(out)
}


#' Get Node Names
#'
#' @template param-x
#' @template param-dots
#'
#' @template author-bk
#'
#'
node_get_names <- function(x, ...) {
  UseMethod("node_get_names")
}

#' @rdname node_get_names
#'
#' @importFrom igraph vertex_attr
#'
#' @export
node_get_names.igraph <- function(x, ...) {
  out <- vertex_attr(x, "name")
  if (is.null(out)) {
    return(as.character(seq_nodes(x)))
  }
  out
}

#' @rdname node_get_names
#'
#' @importFrom igraph vertex_attr
#'
#' @export
node_get_names.network <- function(x, ...) {
  test_val <- x[["val"]][[1L]][["vertex.names"]]
  if (!is.character(test_val)) {
    return(as.character(seq_nodes(x)))
  }
  vapply(x[["val"]], `[[`, character(1L), "vertex.names", USE.NAMES = FALSE)
}


#' Get the Names of Node Attributes
#'
#' @template param-x
#'
#' @return `character()`
#'
#' @template author-bk
#'
#' @export
node_attr_names <- function(x, ...) {
  UseMethod("node_attr_names")
}

#' @rdname node_attr_names
#'
#' @param ignore_na `logical(1L)`, default: `TRUE`. Whether to ignore the `"na"`
#' attribute that `{network}` automatically creates.
#' @template param-dots
#'
#' @examples
#' ig <- example_igraph()
#' node_attr_names(ig)
#'
#' @importFrom igraph vertex_attr_names
#'
#' @export
node_attr_names.igraph <- function(x, ...) {
  vertex_attr_names(x)
}

#' @rdname node_attr_names
#'
#' @examples
#' nw <- example_network()
#' node_attr_names(nw)
#'
#' @export
node_attr_names.network <- function(x, ignore_na = TRUE, ...) {
  out <- unique(unlist(lapply(x[["val"]], names), use.names = FALSE))
  if (ignore_na) {
    return(out[out != "na"])
  }
  out
}

