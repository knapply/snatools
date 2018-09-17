get_metadata <- function(x, full = FALSE) {
  UseMethod("get_metadata")
}

get_metadata.default <- function(x, full = FALSE) {
  out <- list(n_vertices = net_count_vertices(x),
              n_edges = net_count_edges(x),
              is_directed = net_is_directed(x),
              is_bipartite = net_is_bipartite(x),
              n_actors = NULL)
  if (net_is_bipartite(x)) {
    out[["n_actors"]] <- net_count_actors(x)
  }
  if (full) {
    out[["any_multiplex"]] <- net_is_multiplex(x)
    out[["has_loops"]] <- net_has_loops(x)
    out[["has_isolates"]] <- net_has_isolates(x)
  }
  out <- Filter(length, out)
  class(out) <- c("metadata", "list")
  out
}

get_metadata.edgelist <- function(x, full = FALSE) {
  out <- list(n_vertices = attr(x, "n_vertices"),
              n_edges = attr(x, "n_edges"),
              is_directed = attr(x, "is_directed"),
              is_bipartite = attr(x, "is_bipartite"),
              n_actors = NULL)
  if (net_is_bipartite(x)) {
    out[["n_actors"]] <- attr(x, "n_actors")
  }
  if (full) {
    out[["any_multiplex"]] <- net_is_multiplex(x)
    out[["has_loops"]] <- net_has_loops(x)
    out[["has_isolates"]] <- net_has_isolates(x)
  }
  out <- Filter(length, out)
  class(out) <- c("metadata", "list")
  out
}

set_metadata_attr <- function(x, graph) {
  metadata <- get_metadata(graph)
  for (i in seq_along(metadata)) {
    attr(x, names(metadata)[[i]]) <- metadata[[i]]
  }
  x
}

print.net_metadata <- function(x) {
  cat("# A `net_metadata` list.")
  cat("\n")
  print(`colnames<-`(cbind(x), ""))
}

#' @export
net_is_bipartite <- function(x) {
  UseMethod("net_is_bipartite")
}

#' @export
net_is_bipartite.sna_net <- function(x) {
  x[["metadata"]][["is_bipartite"]]
}


#' @export
net_is_bipartite.igraph <- function(x) {
  "type" %in% vrt_attr_names(x)
}


#' @export
net_is_bipartite.network <- function(x) {
  is.numeric(x[["gal"]][["bipartite"]])
}

#' Count the number of vertices belonging to the actor mode of a bipartite graph object.
#' 
#' @param x An `sna_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return `integer` corresponding to the number `x`'s actors.
#' 
#' @examples 
#' library(snatools)
#' 
#' ig <- igraph::bipartite.random.game(6L, 10L, type = "gnp", 0.4)
#' 
#' net_count_actors(ig)
#' 
#' ig %>% 
#'   as_network() %>% 
#'   net_count_actors()
#'
#' ig %>% 
#'   as_sna_net() %>% 
#'   net_count_actors() 
#'   
#' ig %>% 
#'   tidygraph::as_tbl_graph() %>% 
#'   net_count_actors()
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @export
net_count_actors <- function(x) {
  UseMethod("net_count_actors")
}

#' @rdname net_count_actors
#' 
#' @export
net_count_actors.igraph <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` are only applicable to bipartite networks.")
  }
  set_whole_number_storage(length(which(vertex_attr(x, "type"))))
}

#' @rdname net_count_actors
#' 
#' @export
net_count_actors.network <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` only exist in bipartite networks.")
  }
  set_whole_number_storage(x[["gal"]][["bipartite"]])
}

#' @rdname net_count_actors
#' 
#' @export
net_count_actors.sna_net <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` only exist in bipartite networks.")
  }
  set_whole_number_storage(x[["metadata"]][["n_actors"]])
}

#' @rdname net_count_actors
#' 
#' @export
net_count_actors.tbl_graph <- function(x) {
  if (!net_is_bipartite(x)) {
    terminate("`actors` only exist in bipartite networks.")
  }
  as.integer(net_count_actors(as_igraph(x)))
}






#' @export
net_is_directed <- function(x) {
  UseMethod("net_is_directed")
}


#' @export
net_is_directed.sna_net <- function(x) {
  x[["metadata"]][["is_directed"]]
}


#' @importFrom igraph is_directed
#' @export
net_is_directed.igraph <- function(x) {
  is_directed(x)
}


#' @export
net_is_directed.network <- function(x) {
  is_true(x[["gal"]][["directed"]])
}





#' @export
net_count_vertices <- function(x) {
  UseMethod("net_count_vertices")
}


#' @export
net_count_vertices.sna_net <- function(x) {
  set_whole_number_storage(x[["metadata"]][["n_vertices"]])
}


#' @importFrom igraph vcount
#' @export
net_count_vertices.igraph <- function(x) {
  set_whole_number_storage(vcount(x))
}


#' @export
net_count_vertices.network <- function(x) {
  set_whole_number_storage(x[["gal"]][["n"]])
}






#' @export
net_count_edges <- function(x) {
  UseMethod("net_count_edges")
}


#' @export
net_count_edges.sna_net <- function(x) {
  set_whole_number_storage(x[["metadata"]][["n_edges"]])
}


#' @importFrom igraph ecount
#' @export
net_count_edges.igraph <- function(x) {
  set_whole_number_storage(ecount(x))
}

#' @importFrom network network.edgecount
#' @export
net_count_edges.network <- function(x) {
  set_whole_number_storage(network.edgecount(x))
}



