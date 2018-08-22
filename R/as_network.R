#' Conversion to `network` objects.
#' 
#' @param x 
#'     `igraph` ([`igraph::graph`]) or [`tidygraph::tbl_graph`] object.
#' @param ... 
#'     Additional arguments on to other methods (`clean_graph`, `actor_type`).
#' @param clean_graph 
#'     `logical`. Whether to automatically call `clean_graph()` before converting `x`. \cr
#'     Default: `TRUE`
#' @param actor_type 
#'     `logical`. For bipartite graphs, the vertex `"type"` specifying which vertices in 
#'     `x` are to be treated as "actors" in returned `network` object. See __Details__. \cr
#' 
#' @return A [`network::network`] object.
#' 
#' @details `actor_type`
#' * By default, `igraph` represents bipartite graphs using a `logical` vertex attribute named `type`. 
#' * `network` represents bipartite graphs through a count of the vertices in the "actors" 
#' partition. 
#' * `actor_type` provides a way to specify which vertices of an `igraph` object should be
#' represented as the "actors" in the resulting `network` object (as opposed to the vertices 
#' of the second mode, e.g. "events").
#' 
#' @seealso [clean_graph()]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' # `igraph` to `network` ===============================================================
#' ## undirected =========================================================================
#' data("karate", package = "igraphdata")
#' karate
#' 
#' as_network(karate)
#' 
#' ## directed ===========================================================================
#' data("USairports", package = "igraphdata")
#' 
#' USairports
#' 
#' as_network(USairports)
#' 
#' ## bipartite ==========================================================================
#' southern_women_matrix <- matrix(
#'   c(1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
#'     1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
#'     1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
#'     0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
#'     1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
#'     1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,
#'     1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 
#'     0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
#'     0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
#'     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
#'   nrow = 14L, ncol = 18L,
#'   dimnames = list(
#'     c("E1", "E2", "E3", "E4", "E5", "E6", "E7",
#'       "E8", "E9", "E10", "E11", "E12", "E13", "E14"),
#'     c("EVELYN", "LAURA", "THERESA", "BRENDA", "CHARLOTTE", "FRANCES",
#'       "ELEANOR", "PEARL", "RUTH", "VERNE", "MYRNA", "KATHERINE", 
#'       "SYLVIA", "NORA", "HELEN", "DOROTHY", "OLIVIA", "FLORA"))
#'  )
#' 
#' southern_women_ig <- igraph::graph_from_incidence_matrix(southern_women_matrix)
#' 
#' southern_women_ig
#' 
#' vrt_attrs(southern_women_ig)
#' 
#' as_network(southern_women_ig)
#' 
#' ## using `actor_type` to specify bipartite mapping ====================================
#' 
#' sw_transposed <- t(southern_women_matrix)
#' 
#' southern_women_with_actors_false <- igraph::graph_from_incidence_matrix(sw_transposed)
#' 
#' vrt_attrs(southern_women_with_actors_false)
#' 
#' as_network(southern_women_with_actors_false, actor_type = FALSE)
#' 
#' # `tbl_graph` to `network` ============================================================
#' tidy_g <- tidygraph::create_notable("Zachary")
#' 
#' tidy_g
#' 
#' as_network(tidy_g)
#' 
#' @export
as_network <- function(x, ...) {
  UseMethod("as_network")
}

#' @rdname as_network
#' 
#' @export
#' 
as_network.network <- function(x, ...) {
  message("`x` is already a `network` object.")
  x
}

#' @rdname as_network
#' 
#' @export
#' 
as_network.igraph <- function(x, clean_graph = TRUE, actor_type = TRUE) {
  if(clean_graph) {
    x <- clean_graph(x, actor_type)
  }
  vert_attr_names <- vrt_attr_names(x)
  if("vertex.names" %in% vert_attr_names){
    if("name" %in% vert_attr_names) {
      stop('\n`x` is an `igraph` object, but has vertex attributes "vertex.names".\n\n',
         'For `igraph` objects, the attribute name that refers to vertex names should be "name".\n\n',
         'For `network` objects, the attribute name that refers to vertex names should be "vertex.names".\n\n',
         'Since `x` has both, it is ambiguous which attribute should be used for vertex names.\n\n',
         'See `?igraph::set_vertex_attr` and `?igraph::delete_vertex_attr` to remove the ambiguity.',
         call. = FALSE)
    } else {
      warning('\n`x` is an `igraph` object, but has vertex attributes "vertex.names" AND "name".\n\n',
            'For `igraph` objects, the attribute name that refers to vertex names should be "name".\n\n',
            'For `network` objects, the attribute name that refers to vertex names should be "vertex.names".\n\n',
            'Since `x` does not contain a "name" vertex attribute, "vertex.names" is assumed to be intentional.\n\n',
            'See `?igraph::set_vertex_attr` and `?igraph::delete_vertex_attr` to remove the ambiguity and prevent this warning.',
            call. = FALSE)
    }
  }
  graph_attrs <- net_attrs(x)
  vert_attrs <- vrt_attrs(x)
  names(vert_attrs)[names(vert_attrs) == "name"] <- "vertex.names"
  edge_attrs <- edg_attrs(x)
  el <- rep_edgelist(x)
  
  if("type" %in% names(vert_attrs)) {
    if(actor_type) {
      bipartite_arg <- length(which(igraph::V(x)$type))
    } else {
      bipartite_arg <- length(which(!igraph::V(x)$type))
    }
    vert_attrs$type <- NULL
  } else {
    bipartite_arg <- FALSE
  }
  
  args <- list(n = unique(vapply(vert_attrs, length, integer(1), USE.NAMES = FALSE)),
               igraph::is_directed(x),
               hyper = FALSE,
               loops = any(igraph::is.loop(x)),
               multiple = any(igraph::is.multiple(x)),
               bipartite = bipartite_arg)
  
  out <- do.call(network::network.initialize, args)
  
  if(nrow(el)) {
    network::add.edges(out, tail = el[, 1], head = el[, 2]) # assigns invisibly
  }
  if(length(edge_attrs)) {
    for(e_attr in names(edge_attrs)) {
      network::set.edge.attribute(out, e_attr, edge_attrs[[e_attr]]) # assigns invisibly
    }
  }
  if(length(vert_attrs)){
    for(v_attr in names(vert_attrs)){
      network::set.vertex.attribute(out, v_attr, vert_attrs[[v_attr]]) # assigns invisibly
    }
  }
  if(length(graph_attrs)){
    for(g_attr in names(graph_attrs)) {
      network::set.network.attribute(out, g_attr, graph_attrs[[g_attr]]) # assigns invisibly
    }
  }
  
  out
}

#' @rdname as_network
#' 
#' @export
#' 
as_network.default <- function(x, ...) {
  message("A method has not been implemented for `", class(x)[[1]], "` objects...\n",
          "\ttrying `network::as.network()`...\n")
    tryCatch({
        network::as.network(x, ...)
      },
      error = function(e) {
        stop("Objects of `", class(x)[[1]], "` are not supported.\n\n",
             "Use ?igraph::`igraph-package` or ?network::`network-package` to learn about supported objects.",
             call. = FALSE)
      })
}


#' @rdname as_network
#' 
#' @export
#' 
as_network.tbl_graph <- function(x, ...) {
  as_network(as_igraph(x, ...))
}
