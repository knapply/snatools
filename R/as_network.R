#' Conversion to `network` objects.
#' 
#' Accurately map foreign graph data to `network` objects.
#' 
#' @param x `bridge_net`, `igraph` ([`igraph::graph`]), or [`tidygraph::tbl_graph`].
#' @param ... Named arguments passed on to other methods. Currently `.actor`
#' @param .actor `logical`, defaults `TRUE`. Whether to retain the `.actor` vertex attribute
#' of converted bipartite graphs.
#' 
#' @details
#' `as_network()` converts `x` to an intermediate `bridge_net` object that is capable of 
#' mapping metadata, edges, vertices, and attributes (edge, vertex, and graph-level)
#' to a new, valid `network` object. \cr
#' 
#' A note regarding bipartite graph conversion: \cr
#' * `network` represents bipartite graphs differently than `igraph`, and thus `tidygraph`.
#'   + `network` tracks a graph's bipartiteness via the internal indices of each vertex.
#'     + Vertices designated as belonging to a graph's "actor" class come before 
#'       non-"actor" vertices (events, locations, etc.), with "actors" cooresponding 
#'       to the rows of a graph's affiliation matrix representation.
#'   + `igraph` tracks a graph's bipartiteness via a `logical` vertex attribute named 
#'     `type`, with `FALSE` and `TRUE` representing a graph's first and second modes 
#'      respectively.
#'   + The consequence of these very different representations is that converting 
#'     bipartite `igraph`s to `networks` in a way that results in truly _identical_ 
#'     objects is only possible when an `igraph`'s vertices are already sorted by their
#'     `type`` attribute.
#' * To handle this discrepency, intermediate `bridge_net` objects treat vertices whose 
#'   `type` attributes are `TRUE` as "actors", meaning the `igraph`'s vertices are
#'   sorted during conversion to ensure that `TRUE` vertices are first (via 
#'   [`igraph::permute()`]).
#'   + The intention of using `TRUE` vertices is to allow a user to think of an `igraph`'s 
#'     vertices' `type` attribute as indicating whether a vertex is an "actor".
#' * In order to standardize representations, returned bipartite `network` objects 
#'   intentionally retain the intermediate `bridge_net`'s `.actor` vertex attribute (a 
#'   `logical` `vector`).
#'   + `.actor` corresponds to an `igraph`'s `type` vertex attributes, i.e. 
#'     `V(x_igraph)$type` and `x_network %v% ".actor"` will _always_ match.
#'   + If desired, this can be disabled via `as_network()`'s `.actor` parameter, i.e.
#'     `as_network(igraph, .actor = FALSE)`.
#' 
#' @return A [`network::network`] object.
#' 
#' @seealso [as_bridge_net()], [as_igraph()], [intergraph::asNetwork()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' library(igraph, warn.conflicts = FALSE, quietly = TRUE)
#' color_palette <-  colorRampPalette(c("red", "purple", "green", "blue", "cyan"))
#' 
#' # simple, 1-mode igraph to network conversion =========================================
#' ig1 <- random.graph.game(10L, p.or.m = 0.15, directed = TRUE)
#' 
#' coords <- layout_with_fr(ig1)
#' 
#' V(ig1)$name <- seq_along(V(ig1))
#' V(ig1)$color <- color_palette(vcount(ig1))
#' E(ig1)$lab <- seq_along(E(ig1))
#' ig1
#' 
#' nw1 <- as_network(ig1)
#' nw1
#' 
#' # manually comparing original and converted networks ==================================
#' as_bridge_net(ig1)$vertices
#' as_bridge_net(nw1)$vertices
#' identical(as_bridge_net(ig1)$vertices, as_bridge_net(nw1)$vertices)
#' 
#' as_bridge_net(ig1)$edges
#' as_bridge_net(nw1)$edges
#' identical(as_bridge_net(ig1)$edges, as_bridge_net(nw1)$edges)
#' 
#' # converting bipartite igraph to bipartite network ====================================
#' ig2 <- bipartite.random.game(3, 5, "gnp", 0.4)
#' V(ig2)$name <- seq_along(V(ig2))
#' V(ig2)$color <- ifelse(V(ig2)$type, "red", "blue")
#' E(ig2)$lab <- sample(seq_len(ecount(ig2)))
#' ig2
#' 
#' ig_bip_coords <- layout_with_fr(ig2)
#' 
#' V(ig2)$x <- ig_bip_coords[, 1L]
#' V(ig2)$y <- ig_bip_coords[, 2L]
#' 
#' nw2 <- as_network(ig2)
#' nw2
#' 
#' # discarding `.actor` vertex attribute from bipartite graphs ==========================
#' as_network(ig2, .actor = FALSE)
#' 
#' @export
as_network <- function(x, ...) {
  UseMethod("as_network")
}

#' @rdname as_network
#' 
#' @importFrom network add.edges.network network.initialize set.edge.attribute 
#'             set.network.attribute set.vertex.attribute
#' @export
as_network.bridge_net <- function(x, .actor = TRUE) {
  metadata <- x[["metadata"]]
  if (metadata[["is_bipartite"]]) {
    bipartite_arg <- metadata[["n_actors"]]
  } else {
    bipartite_arg <- FALSE
  }
  out <- network.initialize(n = metadata[["n_vertices"]], 
                            directed = metadata[["is_directed"]],
                            hyper = FALSE, 
                            loops = metadata[["any_loops"]], 
                            multiple = metadata[["any_multiplex"]], 
                            bipartite = bipartite_arg)
  if (is.matrix(x[["edges"]])) {
    out <- add.edges.network(out, 
                             tail = x[["edges"]][, ".ego"], 
                             head = x[["edges"]][, ".alter"])
  }
  if (is.data.frame(x[["edges"]])) {
    out <- add.edges.network(out, 
                             tail = x[["edges"]][[".ego"]], 
                             head = x[["edges"]][[".alter"]])
    x[["edges"]][[".ego"]] <- NULL
    x[["edges"]][[".alter"]] <- NULL
  }
  if (length(x[["graph_attributes"]])) {
    for (g in names(x[["graph_attributes"]])) {
      set.network.attribute(out, attrname = g,
                            value = x[["graph_attributes"]][[g]])
    }
  }
  if (is.data.frame(x[["edges"]]) && nrow(x[["edges"]])) {
    for (e in colnames(x[["edges"]])) {
      set.edge.attribute(out, attrname = e, value = x[["edges"]][[e]])
    }
  }
  if (!is.null(x[["vertices"]]) && nrow(x[["vertices"]])) {
    names(x[["vertices"]])[names(x[["vertices"]]) == ".name"] <- "vertex.names"
    if (metadata[["is_bipartite"]] && !.actor) {
      x[["vertices"]][[".actor"]] <- NULL
    }
    for (v in colnames(x[["vertices"]])) {
      set.vertex.attribute(out, attrname = v, value = x[["vertices"]][[v]])
    }
  }
  out
}

#' @rdname as_network
#' 
#' @importFrom igraph is_bipartite
#' @export
as_network.igraph <- function(x, ...) {
  if (is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
  }
  as_network(as_bridge_net(x), ...)
}

#' @rdname as_network
#' 
#' @export
as_network.network <- function(x, ...) {
  x
}

#' @rdname as_network
#' 
#' @export
as_network.tbl_graph <- function(x, ...) {
  as_network(as_bridge_net(x), ...)
}
