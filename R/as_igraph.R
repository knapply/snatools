#' Conversion to `igraph` objects.
#' 
#' Accurately map foreign graph data to `igraph` objects.
#' 
#' @details
#' `as_igraph()` converts `x` to an intermediate `bridge_net` object that is capable of 
#' mapping metadata, edges, vertices, and attributes (edge, vertex, and graph-level)
#' to a new, valid `igraph` object. \cr
#' 
#' @param x `bridge_net` or [`network::network`] object.
#' 
#' @return An `igraph` ([`igraph::graph`]) object.
#' 
#' @seealso [as_bridge_net()], [as_network()], [intergraph::asIgraph()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples
#' library(snatools)
#' suppressPackageStartupMessages(library(network))
#' color_pal <- colorRampPalette(c("red", "green", "blue"))
#' 
#' # simple, 1-mode network to igraph conversion =========================================
#' n_vertices <- 10L
#' adjacency_matrix <- rbinom(n_vertices, 1L, prob = 0.1) %>%
#'   replicate(n = n_vertices, expr = .) %>% 
#'   apply(2L, sample) %>% 
#'   t()
#' 
#' nw1 <- network(adjacency_matrix, directed = TRUE)
#' 
#' nw1 %v% "color" <- color_pal(network.size(nw1))
#' nw1 %e% "lab" <- seq_len(sum(adjacency_matrix))
#' nw1
#' 
#' ig1 <- as_igraph(nw1)
#' ig1
#' 
#' # manually comparing original and converted networks ==================================
#' nw1 %>% vrt_to_df()
#' ig1 %>% vrt_to_df()
#' 
#' identical(vrt_to_df(nw1), vrt_to_df(ig1))
#' 
#' nw1 %>% edg_to_df()
#' ig1 %>% edg_to_df()
#' 
#' identical(edg_to_df(nw1), edg_to_df(ig1))
#' 
#' # converting bipartite network to bipartite igraph ====================================
#' n_actors <- 5L
#' n_events <- 3L
#' affiliation_matrix <- rbinom(n_events, 1L, prob = 0.2) %>%
#'   replicate(n = n_actors, expr = .) %>% 
#'   apply(2L, sample) %>% 
#'   t()
#'   
#' nw2 <- as.network.matrix(affiliation_matrix, matrix.type = "adjacency", 
#'                          bipartite = n_actors)
#' nw2 %v% "color" <- c(rep("red", n_actors), rep("blue", n_events))
#' nw2 %e% "lab" <- seq_len(sum(affiliation_matrix))
#' nw2
#' 
#' ig2 <- as_igraph(nw2)
#' ig2
#' 
#' # comparing objects ===================================================================
#' nw2 %>% vrt_to_df()
#' ig2 %>% vrt_to_df()
#' 
#' identical(vrt_to_df(nw2), vrt_to_df(ig2))
#' 
#' nw2 %>% edg_to_df()
#' ig2 %>% edg_to_df()
#' 
#' identical(edg_to_df(nw2), edg_to_df(ig2))
#'
#' @export
#' 
as_igraph <- function(x) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' 
#' @importFrom igraph add_edges edge_attr<- graph_attr<- make_empty_graph vertex_attr<- V<-
#' @export
as_igraph.bridge_net <- function(x) {
  metadata <- x[["metadata"]]
  out <- make_empty_graph(n = metadata[["n_vertices"]], 
                          directed = metadata[["is_directed"]])
  if (is.matrix(x[["edges"]])) {
    out <- add_edges(out, edges = t(`names<-`(x[["edges"]][, c(".ego", ".alter")], NULL)))
  }
  if (is.data.frame(x[["edges"]])) {
    out <- add_edges(out, edges = t(`names<-`(x[["edges"]][, c(".ego", ".alter")], NULL)))
    x[["edges"]][[".ego"]] <- NULL
    x[["edges"]][[".alter"]] <- NULL
  }
  if (length(x[["net_attrs"]])) {
    graph_attr(out) <- x[["net_attrs"]]
  }
  if (is.data.frame(x[["edges"]]) && nrow(x[["edges"]])) {
    edge_attr(out) <- x[["edges"]]
    
  }
  if (!is.null(x[["vertices"]]) && nrow(x[["vertices"]])) {
    # x[["vertices"]][["type"]] <- x[["vertices"]][[".actor"]]
    if (".actor" %in% names(x[["vertices"]])) {
      names(x[["vertices"]])[names(x[["vertices"]]) == ".actor"] <- "type"
      # x[["vertices"]][[".actor"]] <- NULL
    }
    names(x[["vertices"]])[names(x[["vertices"]]) == ".name"] <- "name"
    # x[["vertices"]][["name"]] <- x[["vertices"]][[".name"]]
    # x[["vertices"]][[".name"]] <- NULL
    vertex_attr(out) <- x[["vertices"]]
  }
  if (is.null(x[["vertices"]]) && x[["metadata"]][["is_bipartite"]]) {
    V(out)$type <- NA
  }
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
  as_igraph(as_bridge_net(x))
}

#' @rdname as_igraph
#' 
#' @export
as_igraph.tbl_graph <- function(x) {
  class(x) <- "igraph"
  `attr<-`(x, "active", NULL)
  x
}
