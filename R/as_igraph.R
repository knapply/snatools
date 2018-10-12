#' Conversion to `igraph` objects.
#' 
#' Convert objects of class `network` to `igraph` objects.
#' 
#' @param x [`network::network`] object.
#' 
#' @return An `igraph` ([`igraph::graph`]) object.
#' 
#' @seealso [as_network()]
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' 
#' data("sampson", package = "ergm")
#' 
#' samplike
#' 
#' samplike %>% 
#'   as_igraph()
#'   
#' # bipartite network to igraph =============================================================
#' southern_women_matrix <- c(
#'   1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
#'   1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#'   0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 
#'   0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1,
#'   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1,
#'   1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0,
#'   1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
#'   1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1,
#'   1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 
#'   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
#'   1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
#'   1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
#'   ) %>% 
#'   matrix(nrow = 18, ncol = 14) %>% 
#'   `rownames<-`(c("EVELYN", "LAURA", "THERESA", "BRENDA", "CHARLOTTE", "FRANCES", 
#'                  "ELEANOR", "PEARL", "RUTH", "VERNE", "MYRNA", "KATHERINE", "SYLVIA", 
#'                  "NORA", "HELEN", "DOROTHY", "OLIVIA", "FLORA"))
#' 
#' sw_nw <- network::as.network.matrix(southern_women_matrix)
#' 
#' sw_nw
#' 
#' sw_nw %>% 
#'   as_igraph()
#'   
#' # networkDynamic to igraph ================================================================
#' data("windsurfers", package = "networkDynamic")
#' 
#' windsurfers
#' 
#' windsurfers %>% 
#'   as_igraph()
#' 
#' @export
as_igraph <- function(x) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' 
#' @importFrom igraph add_edges edge_attr<- graph_attr<- make_empty_graph vertex_attr<- V<-
as_igraph.bridge_net <- function(x) {
  metadata <- x[["metadata"]]
  out <- make_empty_graph(n = metadata[["n_vertices"]], 
                          directed = metadata[["is_directed"]])
  out <- add_edges(out, edges = t(`names<-`(x[["edges"]][, c(".ego", ".alter")], NULL)))
  x[["edges"]][[".ego"]] <- NULL
  x[["edges"]][[".alter"]] <- NULL
  if (ncol(x[["edges"]])) {
    edge_attr(out) <- x[["edges"]]
  }
  if (".actor" %in% names(x[["vertices"]])) {
    x[["vertices"]][["type"]] <- x[["vertices"]][[".actor"]]
  }
  names(x[["vertices"]])[names(x[["vertices"]]) == ".vrt_name"] <- "name"
  vertex_attr(out) <- x[["vertices"]]
  out
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.igraph <- function(x) {
  x
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.network <- function(x) {
  as_igraph.bridge_net(as_bridge_net(x))
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.tbl_graph <- function(x) {
  to_keep <- "class"
  current <- names(attributes(x))
  for (i in seq_along(current)) {
    if (!current[[i]] %in% to_keep) `attr<-`(x, current[[i]], NULL)
  }
  class(x) <- "igraph"
  x
}

