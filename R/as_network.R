#' Conversion to `network` objects.
#' 
#' Convert objects of class `igraph` to `network` objects.
#' 
#' @param x `igraph` ([`igraph::graph`]) or [`tidygraph::tbl_graph`] object.
#' 
#' @return A [`network::network`] object.
#' 
#' @seealso [as_igraph()]
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' 
#' crisis_in_cloister
#' 
#' crisis_in_cloister %>% 
#'   as_network()
#'   
#' florence
#' 
#' florence %>% 
#'   as_network()
#'   
#' # bipartite igraph to network =============================================================
#' southern_women
#' 
#' southern_women %>% 
#'   as_network()
#' 
#' # tidygraph to network ====================================================================
#' suppressPackageStartupMessages(library(tidygraph))
#' 
#' tg <- crisis_in_cloister %>% 
#'   as_tbl_graph()
#'   
#' tg
#' 
#' tg %>% 
#'   as_network()
#' 
#' @export
as_network <- function(x) {
  UseMethod("as_network")
}

#' @rdname as_network
#' 
#' @importFrom network add.edges.network network.initialize set.edge.attribute 
#'             set.network.attribute set.vertex.attribute
#' @export
as_network.bridge_net <- function(x) {
  metadata <- x[["metadata"]]
  if (metadata[["is_bipartite"]]) {
    bipartite_arg <- metadata[["n_actors"]]
  } else {
    bipartite_arg <- FALSE
  }
  out <- network.initialize(n = metadata[["n_vertices"]], 
                            directed = metadata[["is_directed"]],
                            hyper = FALSE, 
                            loops = metadata[["has_loops"]], 
                            multiple = metadata[["is_multiplex"]], 
                            bipartite = bipartite_arg)
  out <- add.edges.network(out, 
                           tail = x[["edges"]][[".ego"]], 
                           head = x[["edges"]][[".alter"]])
  x[["edges"]][[".ego"]] <- NULL
  x[["edges"]][[".alter"]] <- NULL
  # TODO decide if non-structural graph-attributes even be considered?
  # if (length(x[["net_attrs"]])) {
  #   for (g in names(x[["net_attrs"]])) {
  #     set.network.attribute(out, attrname = g,
  #                           value = x[["net_attrs"]][[g]])
  #   }
  # }
  if (ncol(x[["edges"]])) {
    for (e in colnames(x[["edges"]])) {
      set.edge.attribute(out, attrname = e, value = x[["edges"]][[e]])
    }
  }
  if (!is.null(x[["vertices"]]) && nrow(x[["vertices"]])) {
    names(x[["vertices"]])[names(x[["vertices"]]) == ".vrt_name"] <- "vertex.names"
    set.vertex.attribute(out, names(x[["vertices"]]), value = x[["vertices"]])
  }
  out
}

#' @rdname as_network
#' 
#' @export
as_network.igraph <- function(x) {
  as_network.bridge_net(as_bridge_net(x))
}

#' @rdname as_network
#' 
#' @export
as_network.network <- function(x) {
  x
}

#' @rdname as_network
#' 
#' @export
as_network.tbl_graph <- function(x) {
  as_network.bridge_net(as_bridge_net(as_igraph.tbl_graph(x)))
}

