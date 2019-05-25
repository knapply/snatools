#' Count the number of edges in a network.
#'
#' @template param-net
#' 
#' @return `numeric` scalar
#' 
#' @template author-bk
#' 
#' @export
edge_count <- function(.net) {
  UseMethod("edge_count")
}

#' @rdname edge_count
#' 
#' @importFrom igraph ecount
#' 
#' @export
edge_count.igraph <- function(.net) {
  ecount(.net)
}

#' @rdname edge_count
#' 
#' @importFrom  network network.edgecount
#' 
#' @export
edge_count.network <- function(.net) {
  network.edgecount(.net)
}

edge_seq <- function(.net) {
  seq_len(edge_count(.net))
}

#' Get the names of a network's edge attributes.
#'
#' @template param-net
#' 
#' @return `character` `vector`
#' 
#' @template author-bk
#' 
#' @export
edge_get_attr_names <- function(.net) {
  UseMethod("edge_get_attr_names")
}

#' @rdname edge_get_attr_names
#' 
#' @importFrom igraph edge_attr_names
#' 
#' @export
edge_get_attr_names.igraph <- function(.net) {
  edge_attr_names(.net) %{}% NULL
}

#' @rdname edge_get_attr_names
#' 
#' @export
edge_get_attr_names.network <- function(.net) {
  out <- unique(
    .flatten_chr(
      .map(
        .map(.net[["mel"]], `[[`, "atl"),
        names
      )
    )
  )
  
  out[out != "na"] %{}% NULL
}


#' Does an edge attribute exist?
#'
#' @template param-net
#' @template param-edge_attr
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @export
edge_attr_exists <- function(.net, .edge_attr) {
  .edge_attr %in% edge_get_attr_names(.net)
}

#' Extract an edge attribute.
#'
#' @template param-net
#' @template param-edge_attr
#' @template param-edge_index
#' 
#' @return `vector` (typically)
#' 
#' @template author-bk
#' 
#' @export
edge_get_attr <- function(.net, .edge_attr, .edge_index = edge_seq(.net)) {
  if (!.is_scalar_chr(.edge_attr)) {
    .stop("`.edge_attr` must be a scalar character.")
  }
  if (!edge_attr_exists(.net, .edge_attr)) {
    .net <- deparse(substitute(.net))
    .stop("`{.edge_attr}` is not a valid edge attribute for `{.net}`.")
  }
  if (.is_empty(.edge_index)) {
    .warning("`.edge_index` is empty.")
    return(NULL)
  }
  if (!is.numeric(.edge_index) && !is.logical(.edge_index)) {
    .stop("`.edge_index` must be a `numeric` or `logical` `vector`")
  }
  if (length(.edge_index) != edge_count(.net)) {
    mess <- ifelse(length(.edge_index) > edge_count(.net), "longer", "shorter")
    .net <- deparse(substitute(.net))

    .stop("`.edge_index` is {mess} than the number of edges in `{.net}`")
  }

  UseMethod("edge_get_attr")
}

#' @rdname edge_get_attr
#' 
#' @importFrom igraph edge_attr
#' 
#' @export
edge_get_attr.igraph <- function(.net, .edge_attr, .edge_index = edge_seq(.net)) {
  if (is.logical(.edge_index)) {
    .edge_index <- which(.edge_index)
  }
  edge_attr(graph = .net, name = .edge_attr, index = .edge_index)
}

#' @rdname edge_get_attr
#' 
#' @export
edge_get_attr.network <- function(.net, .edge_attr, .edge_index = edge_seq(.net)) {
  if (is.logical(.edge_index)) {
    .edge_index <- which(.edge_index)
  }
  .flatten(
    .map(.map(.net[["mel"]], `[[`, "atl")[.edge_index], 
         function(x) x[[.edge_attr]] %||% NA)
  )
}
