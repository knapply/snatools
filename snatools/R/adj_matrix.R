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
#' @examples 
#' library(snatools)
#' 
#' florence %>% 
#'   edg_subset(relation == "marriage") %>% 
#'   rep_as_adj_matrix() %>% 
#'   `colnames<-`(rep("", ncol(.)))
#'   
#' crisis_in_cloister %>% 
#'   edg_subset(time == 1 & affect == 3) %>% 
#'   rep_as_adj_matrix() %>% 
#'   `colnames<-`(rep("", ncol(.)))
#'   
#' florence %>% 
#'   rep_as_adj_matrix(edg_attr = "relation") %>% 
#'   `colnames<-`(rep("", ncol(.)))
#' 
#' @importFrom methods as
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
  out <- matrix(rep(as(NA, typeof(fill)), n_vertices * n_vertices),
                nrow = n_vertices, ncol = n_vertices)
  el <- get_el(x)
  if (is.null(edg_attr) & net_is_multiplex(x)) {
    warning("`x` is multiplex with no `edg_attr` specified. Duplicate edges will be discarded")
    el <- unique.matrix(el)
  }
  out[el] <- fill
  if (is.numeric(out)) {
    st_mode <- storage.mode(out)
    out[is.na(out)] <- as(0, st_mode)
  }
  if (is.null(dim_names)) {
    return(out)
  }
  `dimnames<-`(out, list(dim_names, dim_names))
}
