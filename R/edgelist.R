#' Construct a graph object's edgelist representation.
#' 
#' @template graph-param
#' @template use_names-param
#' @template vrt_attr-fill-param
#' 
#' @return `matrix` with `net_count_edges(x)` rows and 2 columns (`.ego` and `.alter`).
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [igraph::as_edgelist()], [network::as.matrix.network.edgelist()]
#' 
#' @export
rep_as_edgelist <- function(x, use_names = TRUE, vrt_attr = NULL) {
  validate_args(x = x, vrt_attr = vrt_attr, validate_graph = TRUE)
  if (!is.null(vrt_attr)) {
    fill <- vrt_get_attr(x, vrt_attr)
  } else if (use_names) {
    fill <- vrt_get_names(x)
  } else {
    fill <- NULL
  }
  out <- get_el(x)
  if (!is.null(fill)) {
    out <- matrix(fill[out], ncol = 2L)
  }
  `colnames<-`(out, c(".ego", ".alter"))
}

get_el <- function(x) {
  UseMethod("get_el")
}

get_el.network <- function(x) {
  `storage.mode<-`(cbind(unlist(lapply(x[["mel"]], `[[`, "outl")),
                         unlist(lapply(x[["mel"]], `[[`, "inl"))),
                   "integer")
}

#' @importFrom igraph as_edgelist
get_el.igraph <- function(x) {
  `storage.mode<-`(igraph::as_edgelist(x, names = FALSE), "integer")
}

get_el.tbl_graph <- function(x) {
  get_el.igraph(as_igraph.tbl_graph(x))
}


