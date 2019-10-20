#' Build an Example Adjacency Matrix
#'
#' @template param-directed
#' @template param-loops
#'
#' @template author-bk
#'
#' @examples
#' example_adj_mat()
#'
#' example_adj_mat(directed = TRUE)
#'
#' example_adj_mat(loops = TRUE)
#'
#' example_adj_mat(directed = TRUE, loops = TRUE)
#'
#' @export
example_adj_mat <- function(directed = FALSE, loops = FALSE) {
  init <- structure(c(0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1,
                      1, 0, 0, 1, 0, 1, 0, 0), .Dim = c(5L, 5L))
  if (loops) {
    diag(init) <- 1
  }
  if (directed) {
    return(init)
  }
  init[] <- init + t(init)
  init %/% 2
}


#' Build an Example `<igraph>`
#'
#' @template param-directed
#' @template param-loops
#'
#' @examples
#' example_igraph()
#'
#' example_igraph(directed = TRUE)
#'
#' example_igraph(loops = TRUE)
#'
#' example_igraph(directed = TRUE, loops = TRUE)
#'
#' @importFrom igraph ecount edge_attr<- graph_from_adjacency_matrix vertex_attr<-
#' @export
example_igraph <- function(directed = FALSE, loops = FALSE) {
  out <- graph_from_adjacency_matrix(
    adjmatrix = example_adj_mat(directed, loops),
    mode = if (directed) "directed" else "undirected"
  )
  vertex_attr(out, "color") <- c(
    "red", "yellow", "green", "lightblue", "salmon"
  )
  vertex_attr(out, "size") <- seq_len(vcount(out)) * 2
  vertex_attr(out, "x") <- c(1, 5, 2, 3, 4)
  vertex_attr(out, "y") <- rev(c(1, 5, 2, 3, 4))
  vertex_attr(out, "name") <- letters[seq_len(vcount(out))]

  list_attr <- mapply(c, LETTERS, letters, SIMPLIFY = FALSE, USE.NAMES = FALSE
                      )[seq_len(vcount(out))]
  list_attr[[1L]] <- list(NULL)
  vertex_attr(out, "list_attr") <- list_attr

  hetero_attr <- c(as.list(seq_len(vcount(out) - 1)), "a")
  vertex_attr(out, "hetero_attr") <- hetero_attr

  edge_attr(out, "weight") <- seq_len(ecount(out))

  out
}


#' Build an Example `<network>`
#'
#' @template param-directed
#' @template param-loops
#'
#' @examples
#' example_network()
#'
#' example_network(directed = TRUE)
#'
#' example_network(loops = TRUE)
#'
#' example_network(directed = TRUE, loops = TRUE)
#'
#' @importFrom network as.network.matrix network.edgecount network.size
#' @importFrom network set.edge.attribute set.vertex.attribute
#' @export
example_network <- function(directed = FALSE, loops = FALSE) {
  out <- as.network.matrix(
    x = example_adj_mat(directed, loops),
    matrix.type = "adjacency",
    directed = directed,
    loops = loops,
    hyper = FALSE,
    multiple = FALSE
  )

  set.vertex.attribute(
    out, "color",
    value = c("red", "yellow", "green", "lightblue", "salmon")
  )
  set.vertex.attribute(
    out, "size",
    value = seq_len(network.size(out)) * 2
  )
  set.vertex.attribute(
    out, "x",
    value = c(1, 5, 2, 3, 4)
  )
  set.vertex.attribute(
    out, "y",
    value = rev(c(1, 5, 2, 3, 4))
  )
  set.vertex.attribute(
    out, "vertex.names", value = letters[seq_len(network::network.size(out))]
  )

  list_attr <- mapply(c, LETTERS, letters, SIMPLIFY = FALSE, USE.NAMES = FALSE
                      )[seq_len(network.size(out))]
  list_attr[[1L]] <- list(NULL)
  set.vertex.attribute(out, "list_attr", value = list_attr)

  hetero_attr <- c(as.list(seq_len(network.size(out) - 1)), "a")
  set.vertex.attribute(out, "hetero_attr", value = hetero_attr)

  set.edge.attribute(
    out, "weight", value = seq_len(network.edgecount(out))
  )

  out
}


# plot_example <- function(x, ...) {
#   UseMethod("plot_example")
# }
# plot_example.igraph <- function(x, ...) {
#   plot(x)
# }
# plot_example.network <- function(x, ...) {
#   network::plot.network(
#     x, displaylabels = TRUE,
#     vertex.cex = network::get.vertex.attribute(x, "size") * 0.25,
#     coord = cbind(network::get.vertex.attribute(x, "x"),
#                   network::get.vertex.attribute(x, "y")),
#     jitter = FALSE,
#     vertex.col = network::get.vertex.attribute(x, "color"),
#     edge.col = "gray"
#
#   )
# }




