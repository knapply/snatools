#' Construct a graph object's edgelist representation.
#' 
#' @template graph-param
#' @template use_names-param
#' @template vrt_attr-param
#' @template edg_attr-fill-param
#' 
#' @return `matrix` with `net_count_vertices(x)` rows and columns
#' 
#' @template bknapp-author
#' 
#' @seealso [igraph::as_adjacency_matrix()], [network::as.matrix.network.adjacency()]
#' 
#' @export
rep_as_adj_matrix <- function(x, use_names = TRUE, vrt_attr = NULL, edg_attr = NULL) {
  validate_args(x = x, validate_graph = TRUE)
  if (use_names && is.null(vrt_attr)) {
    dim_names <- vrt_get_names(x)
  } else if (!is.null(vrt_attr)) {
    dim_names <- vrt_get_attr(x, vrt_attr)
  } else {
    dim_names <- NULL
  }
  if (!is.null(edg_attr)) {
    fill <- edg_get_attr(x, edg_attr)
  } else {
    fill <- 1L
  }
  n_vertices <- net_count_vertices(x)
  init <- vector(typeof(fill), n_vertices * n_vertices)
  el <- get_el(x)
  if (net_is_multiplex(x)) {
    warning("`x` is multiplex. Duplicate edges will be discarded")
    el <- unique.matrix(el)
  }
  out[el] <- fill
  if (is.null(dim_names)) {
    return(out)
  }
  `dimnames<-`(out, list(dim_names, dim_names))
}