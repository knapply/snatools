#' Conversion to `network` objects.
#' 
#' @param x 
#'     `igraph` ([`igraph::graph`]) or [`tidygraph::tbl_graph`] object.
#' @param ... 
#'     Additional arguments passed on to other methods (currently `actor_type`).
#' @param actor_type 
#'     `logical`. For bipartite graphs, the vertex `"type"` specifying which vertices in 
#'     `x` are to be treated as "actors" in returned `network` object. See Details. \cr
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
#' @seealso [as_igraph()], [intergraph::asNetwork()]
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
#' vrt_get_attrs(southern_women_ig)
#' 
#' as_network(southern_women_ig)
#' 
#' ## using `actor_type` to specify bipartite mapping ====================================
#' 
#' sw_transposed <- t(southern_women_matrix)
#' 
#' southern_women_with_actors_false <- igraph::graph_from_incidence_matrix(sw_transposed)
#' 
#' vrt_get_attrs(southern_women_with_actors_false)
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
  x
}

#' @rdname as_network
#' 
#' @export
as_network.network_dataset <- function(x) {
  as_network(as_igraph(x))
}

#' @rdname as_network
#' 
#' @importFrom igraph V vcount
#' @importFrom network add.edges delete.network.attribute delete.vertex.attribute 
#' network.initialize set.edge.attribute set.network.attribute set.vertex.attribute
#' @export
as_network.igraph <- function(x, actor_type = TRUE) {
  vrt_attr_names <- vrt_get_attr_names(x)
  if("vertex.names" %in% vrt_attr_names){
    if("name" %in% vrt_attr_names) {
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
  net_attrs <- net_get_attrs(x)
  vrt_attrs <- vrt_get_attrs(x)
  if("name" %in% names(vrt_attrs)) {
    names(vrt_attrs)[names(vrt_attrs) == "name"] <- "vertex.names"
  }
  edg_attrs <- edg_get_attrs(x)
  el <- rep_edgelist(x)

  if(has_loops(x)) {
    loops_arg <- TRUE
  } else {
    loops_arg <- NULL
  }
  if(edg_any_parallel(x)) {
    multiple_arg <- TRUE
  } else {
    multiple_arg <- NULL
  }
  if("type" %in% names(vrt_attrs)) {
    if(actor_type) {
      bipartite_arg <- length(which(V(x)$type))
    } else {
      bipartite_arg <- length(which(!V(x)$type))
    }
    names(vrt_attrs) <- txt_replace(names(vrt_attrs), "type", "is_actor")
  } else {
    bipartite_arg <- NULL
  }
  args <- list(n = vcount(x),
               directed = net_is_directed(x),
               hyper = FALSE,
               loops = loops_arg,
               multiple = multiple_arg,
               bipartite = bipartite_arg)
  args <- Filter(length, args)
  
  out <- do.call(network.initialize, args)
  # `network.initialize` MAY auto create loops, multiple, bipartite, and vertex.names
  # inspect and drop attrs not present in `x`
  if("loops" %in% net_get_attr_names(out, drop_metadata = FALSE)) {
    if(!"loops" %in% net_get_attr_names(x)) {
      delete.network.attribute(out, "loops")
      out$gal$loops <- NULL
    }
  }
  # if("multiple" %in% net_get_attr_names(out, drop_metadata = FALSE)) {
  #   if(!"multiple" %in% net_get_attr_names(x)) {
  #     network::delete.vertex.attribute(out, "multiple")
  #     out$gal$multiple <- NULL
  #   }
  # }
  if("bipartite" %in% net_get_attr_names(out, drop_metadata = FALSE)) {
    if(!"bipartite" %in% net_get_attr_names(x)) {
      if(!net_is_bipartite(x)) {
        delete.network.attribute(out, "bipartite")
        out$gal$bipartite <- NULL
      }
    }
  }
  if("vertex.names" %in% vrt_get_attr_names(out)) { 
    delete.vertex.attribute(out, "vertex.names")
  }
  if(nrow(el)) {
    if(args$directed) {
      add.edges(out, tail = el[, 1], head = el[, 2]) # assigns invisibly
    } else {
      add.edges(out, tail = el[, 2], head = el[, 1]) # assigns invisibly
    }
  }
  if(length(edg_attrs)) {
    for(e_attr in names(edg_attrs)) {
      set.edge.attribute(out, e_attr, edg_attrs[[e_attr]]) # assigns invisibly
    }
  }
  if(length(vrt_attrs)){
    for(v_attr in names(vrt_attrs)){
      set.vertex.attribute(out, v_attr, vrt_attrs[[v_attr]]) # assigns invisibly
    }
  }
  if(length(net_attrs)){
    for(g_attr in names(net_attrs)) {
      set.network.attribute(out, g_attr, net_attrs[[g_attr]]) # assigns invisibly
    }
  }
  
  out
}

#' @rdname as_network
#' 
#' @export
as_network.tbl_graph <- function(x, ...) {
  if(!requireNamespace("tidygraph", quietly = TRUE)) {
    stop('The `tidygraph` package is required for this functionality. 
          Install it via `install.packages("tidygraph")`', call. = FALSE)
  }
  as_network(tidygraph::as.igraph(x, ...))
}

