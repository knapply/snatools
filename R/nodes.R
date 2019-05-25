#' Count the number of nodes in a network.
#'
#' @template param-net
#' 
#' @return `integer` scalar
#' 
#' @template author-bk
#' 
#' @export
node_count <- function(.net) {
  UseMethod("node_count")
}

#' @rdname node_count
#' 
#' @importFrom igraph vcount
#' 
#' @export
node_count.igraph <- function(.net) {
  vcount(.net)
  # unclass(.net)[[1L]]
}

#' @rdname node_count
#' 
#' @export
node_count.network <- function(.net) {
  .net[["gal"]][["n"]]
}

node_seq <- function(.net) {
  seq_len(node_count(.net))
}

#' Get the names of a network's node attributes.
#'
#' @template param-net
#' 
#' @return `character` `vector`
#' 
#' @template author-bk
#' 
#' @export
node_get_attr_names <- function(.net) {
  UseMethod("node_get_attr_names")
}

#' @rdname node_get_attr_names
#' 
#' @importFrom igraph vertex_attr_names
#' 
#' @export
node_get_attr_names.igraph <- function(.net) {
  vertex_attr_names(.net)
}

#' @rdname node_get_attr_names
#' 
#' @export
node_get_attr_names.network <- function(.net) {
  out <- .flatten_chr(
    unique(
      .map(.net[["val"]], names)
    )
  )

  out[out != "na"]
}


#' Does a node attribute exist?
#'
#' @template param-net
#' @template param-node_attr
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @export
node_attr_exists <- function(.net, .node_attr) {
  .node_attr %in% node_get_attr_names(.net)
}


#' Extract a node attribute.
#'
#' @template param-net
#' @template param-node_attr
#' @template param-node_index
#' 
#' @return `vector` (typically)
#' 
#' @template author-bk
#' 
#' @export
node_get_attr <- function(.net, .node_attr, .node_index = node_seq(.net)) {
  if (!.is_scalar_chr(.node_attr)) {
    .stop("`.node_attr` must be a scalar character.")
  }
  if (!edge_attr_exists(.net, .node_attr)) {
    .net <- deparse(substitute(.net))
    .stop("`{.node_attr}` is not a valid node attribute for `{.net}`.")
  }
  if (.is_empty(.node_index)) {
    .warning("`.node_index` is empty.")
    return(NULL)
  }
  if (!is.numeric(.node_index) && !is.logical(.node_index)) {
    .stop("`.node_index` must be a `numeric` or `logical` `vector`")
  }
  if (length(.node_index) != node_count(.net)) {
    mess <- ifelse(length(.node_index) > node_count(.net), "longer", "shorter")
    .net <- deparse(substitute(.net))
    
    .stop("`.node_index` is {mess} than the number of nodes in `{.net}`")
  }

  UseMethod("node_get_attr")
}

#' @rdname node_get_attr
#' 
#' @importFrom igraph vertex_attr
#' 
#' @export
node_get_attr.igraph <- function(.net, .node_attr, .node_index = node_seq(.net)) {
  vertex_attr(graph = .net, name = .node_attr, index = .node_index)
}

#' @rdname node_get_attr
#' 
#' @export
node_get_attr.network <- function(.net, .node_attr, .node_index = node_seq(.net)) {
  .flatten(
    .map(.net[["val"]][.node_index], 
         function(x) x[[.node_attr]] %||% NA)
  )
}

#' Extract node names.
#'
#' @template param-net
#' 
#' @return `vector` corresponding to node names. See Details.
#' 
#' @details 
#' * `igraph` uses `"name"` to name its vertices. `node_get_names()` returns the `"name"`
#'   attribute if present. If not, `node_get_names()` returns `node_seq(.net)` 
#' * `network` uses `"vertex.names"` to names it vertices. `node_get_names()` always 
#'   returns the `"vertex.names"` attribute.
#' 
#' @template author-bk
#' 
#' @export
node_get_names <- function(.net) {
  UseMethod("node_get_names")
}

#' @rdname node_get_names
#' 
#' @importFrom igraph vertex_attr
#' 
#' @export
node_get_names.igraph <- function(.net) {
  vertex_attr(.net, "name") %{}% node_seq(.net)
}

#' @rdname node_get_names
#' 
#' @export
node_get_names.network <- function(.net) {
  node_get_attr(.net, "vertex.names")
}





