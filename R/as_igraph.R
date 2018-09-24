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
#' color_palette <-  colorRampPalette(c("red", "purple", "green", "blue", "cyan"))
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
#' coords <- network.layout.fruchtermanreingold(nw1, layout.par = NULL)
#' 
#' nw1 %v% "color" <- color_palette(network.size(nw1))
#' nw1 %e% "lab" <- seq_len(sum(adjacency_matrix))
#' nw1
#' 
#' ig1 <- as_igraph(nw1)
#' ig1
#' 
#' # manually comparing original and converted networks ==================================
#' as_bridge_net(nw1)$vertices
#' as_bridge_net(ig1)$vertices
#' identical(as_bridge_net(nw1)$vertices, as_bridge_net(ig1)$vertices)
#' 
#' as_bridge_net(nw1)$edges
#' as_bridge_net(ig1)$edges
#' identical(as_bridge_net(nw1)$edges, as_bridge_net(ig1)$edges)
#' 
#' # converting bipartite network to bipartite igraph ====================================
#' n_actors <- 5L
#' n_events <- 3L
#' affiliation_matrix <- rbinom(n_events, 1L, prob = 0.2) %>%
#'   replicate(n = n_actors, expr = .) %>% 
#'   apply(2L, sample) %>% 
#'   t()
#'   
#' nw2 <- network(affiliation_matrix, bipartite = n_actors)
#' 
#' nw_bip_coords <- network.layout.fruchtermanreingold(nw2, layout.par = NULL)
#' 
#' nw2 %v% "color" <- c(rep("red", n_actors), rep("blue", n_events))
#' nw2 %v% "x" <- nw_bip_coords[, 1L]
#' nw2 %v% "y" <- nw_bip_coords[, 2L]
#' nw2 %e% "lab" <- seq_len(sum(affiliation_matrix))
#' nw2
#' 
#' ig2 <- as_igraph(nw2)
#' ig2
#' 
#' # comparing objects ===================================================================
#' as_bridge_net(nw2)$vertices
#' as_bridge_net(ig2)$vertices
#' identical(as_bridge_net(nw2)$vertices, as_bridge_net(ig2)$vertices)
#' 
#' as_bridge_net(nw2)$edges
#' as_bridge_net(ig2)$edges
#' identical(as_bridge_net(nw2)$edges, as_bridge_net(ig2)$edges)
#' 
#' # visual comparison ===================================================================
#' plot_net <- function(x, coords = NULL, main = NULL, v_cex = 0L, v_lab_cex = NULL, 
#'                      e_col = "lightgray", e_lab_cex = NULL, e_lab_col = "black", 
#'                      arw_cex = NULL) {
#'   if (class(x) == "igraph") {
#'     v_lab_cols <- igraph::V(x)$color
#'     if(all(is.na(v_lab_cols))) v_lab_cols <- NULL
#'     args <- list(x = x, layout = coords, main = main,
#'                  vertex.size = v_cex, vertex.label.color = v_lab_cols, 
#'                  vertex.label.cex = v_lab_cex, edge.color = e_col, 
#'                  edge.label = igraph::E(x)$lab, edge.label.color = e_lab_col,
#'                  edge.label.size = e_lab_cex, edge.arrow.size = arw_cex)
#'     args <- Filter(length, args)
#'     do.call(plot, args)
#'   } else if (class(x) == "network") {
#'     v_lab_cols <- network::get.vertex.attribute(x, "color")
#'     if(all(is.na(v_lab_cols))) v_lab_cols <- NULL
#'     args <- list(x = x, coord = coords, main = main,
#'                  label = "vertex.names", vertex.cex = v_cex, label.col = v_lab_cols, 
#'                  label.cex = v_lab_cex, edge.col = e_col, edge.label = "lab", 
#'                  edge.label.col = e_lab_col, edge.label.cex = e_lab_cex, label.pos = 5,
#'                  arrowhead.cex = arw_cex)
#'     args <- Filter(length, args)
#'     do.call(plot, args)
#'   } else stop("`x` is not an `igraph` or `network` object.", call. = FALSE)
#' }
#' 
#' par(mfrow = c(1, 2))
#' plot_net(nw1, coords, v_lab_cex = 1.75, e_lab_cex = 0.75, arw_cex = 2.5,
#'          main = "original network")
#' plot_net(ig1, coords, v_lab_cex = 1.75, e_lab_cex = 0.75, arw_cex = 0.65,
#'          main = "post-conversion igraph")
#' 
#' ig_bip_coords <- cbind(igraph::V(ig2)$x, igraph::V(ig2)$y)
#' 
#' plot_net(nw2, nw_bip_coords, v_lab_cex = 1.25,
#'          e_lab_col = "black", e_lab_cex = 0.75, main = "original\nbipartite network")
#' plot_net(ig2, ig_bip_coords, v_lab_cex = 1.5, e_lab_col = "black", 
#'          e_lab_cex = 0.75, main = "post-conversion\nbipartite igraph")
#'
#' par(mfrow = c(1, 1))
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
    x[["vertices"]][["type"]] <- x[["vertices"]][[".actor"]]
    x[["vertices"]][[".actor"]] <- NULL
    x[["vertices"]][["name"]] <- x[["vertices"]][[".name"]]
    x[["vertices"]][[".name"]] <- NULL
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
