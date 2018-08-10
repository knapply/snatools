#' #' Strict object comparison.
#' #'
#' #' * This infix operator
#' #'     + provides stricter comparisons than `==`.
#' #'     + prevents silent type conversions.
#' #'     + offers a method to easily compare `igraph` objects.
#' #'
#' #' @param lhs left-hand side
#' #' @param rhs right-hand side
#' #'
#' #' @return `logical` Whether `lhs` and `rhs` are identical.
#' #'
#' #' @name %==%
#' #'
#' #' @rdname strict-compare
#' #'
#' #' @seealso [`base::identical()`]
#' #'
#' #' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' #'
#' #' @examples
#' #'
#' #' TRUE == 1
#' #' TRUE %==% 1
#' #'
#' #' NA == NA_character_
#' #' NA %==% NA_character_
#' #'
#' #' NULL == NA_integer_
#' #' NULL %==% NA_integer_
#' #'
#' #' NA == "NA"
#' #' NA %==% "NA"
#' #'
#' #' igraph::graph("Zachary") %==% igraph::graph("Zachary")
#' #'
#' #' @export
#' `%==%` <- function(lhs, rhs) {
#'   UseMethod("%==%")
#' }
#' 
#' 
#' #' @rdname strict-compare
#' #'
#' #' @export
#' #'
#' `%==%.default` <- function(lhs, rhs) {
#'   identical(lhs, rhs)
#' }
#' 
#' #' @rdname strict-compare
#' #'
#' #' @export
#' #'
#' `%==%.igraph` <- function(lhs, rhs) {
#'   identical(unclass(lhs)[seq_len(9)], unclass(rhs)[seq_len(9)])
#' }


# sna_as_edgelist.network <- function(nw) {
#   if(!nw$gal$directed) {
#     return(cbind(vapply(nw$mel, function(x) x[["inl"]], numeric(1)),
#                  vapply(nw$mel, function(x) x[["outl"]], numeric(1))))
#     }
#   cbind(vapply(nw$mel, function(x) x[["outl"]], numeric(1)),
#         vapply(nw$mel, function(x) x[["inl"]], numeric(1))) 
# }

# vrt_names <- vapply(nw$val, function(x) x[["vertex.names"]], character(1))
# 
# nw <- build_test_graph("nw")
# out <- cbind(vapply(nw$mel, function(x) x[["inl"]], numeric(1)),
#              vapply(nw$mel, function(x) x[["outl"]], numeric(1)))
# 
# matrix(vrt_names[out], ncol = 2) %>% head()
# vrt_names %>% head()
# nw %>% network::as.matrix.network.edgelist() %>% head()

#' Clean a graph object's attributes in a standardized fashion.
#' 
#' @param x An `igraph` or `network` object.
#' 
#' @return A cleaned version of `x`.
#' 
#' @export
sna_standardize_graph <- function(x) {
  UseMethod("sna_standardize_graph")
}

#' @rdname sna_standardize_graph
#' 
#' @export
#' 
sna_standardize_graph.igraph <- function(ig) {
  graph_attrs <- sna_get_graph_attrs(ig)
  edge_attrs <- sna_get_edge_attrs(ig)
  vert_attrs <- sna_get_vert_attrs(ig)
  names(vert_attrs)[names(vert_attrs) == "vertex.names"] <- "name"
  
  igraph::graph_attr(ig) <- graph_attrs
  igraph::edge_attr(ig) <- edge_attrs
  igraph::vertex_attr(ig) <- vert_attrs
  
  ig
}

#' @rdname sna_standardize_graph
#' 
#' @export
#' 
sna_standardize_graph.network <- function(nw) {
  vert_attrs <- sna_get_vert_attrs(nw)
  names(vert_attrs)[names(vert_attrs) == "name"] <- "vertex.names"
  graph_attrs <- sna_get_graph_attrs(nw)
  edge_attrs <- sna_get_edge_attrs(nw)
  el <- sna_as_edgelist(nw)
  
  args <- list(n = nw$gal$n,
               directed = nw$gal$directed,
               hyper = nw$gal$hyper,
               loops = nw$gal$loops,
               multiple = nw$gal$multiple,
               bipartite = nw$gal$bipartite)
  
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
  
  if(is.numeric(out$gal$bipartite)) {
    out$gal$loops <- FALSE
  }

  out
}
