#' Construct a graph object's adjacency list representation.
#' 
#' @template graph-param
#' @template use_names-param
#' @template vrt_attr-param
#' 
#' @return `list`, where `names` and values correspond to vertex names, indices, 
#' or attributes, depending on arguments provided.
#' 
#' @template bknapp-author
#' 
#' @seealso [igraph::as_adj_list()]
#' 
#' @export
rep_as_adj_list <- function(x, use_names = TRUE, vrt_attr = NULL) {
  validate_args(x, validate_graph = TRUE)
  if (use_names && is.null(vrt_attr)) {
    fill <- vrt_get_names(x)
  } else if (!is.null(vrt_attr)) {
    fill <- vrt_get_attr(x, vrt_attr)
  } else {
    fill <- NULL
  }
  adj_list <- get_adj_list(x)
  if (is.null(fill)) {
    return(lapply(adj_list, function(edg_seq) fill[edg_seq]))
  }
  names(adj_list) <- fill
  lapply(adj_list, function(edg_seq) fill[edg_seq])
}

get_adj_list <- function(x) {
  el <- get_el(x)
  egos <- seq_len(net_count_vertices(x))
  lapply(egos, function(ego) el[el[, 1L] == ego, 2L])
}
