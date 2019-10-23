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
  out <- structure(c(0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1,
                      1, 0, 0, 1, 0, 1, 0, 0), .Dim = c(5L, 5L))
  if (loops) {
    diag(out) <- 1
  }
  if (directed) {
    return(out)
  }
  out[] <- out + t(out)
  out %/% 2
}

#' Build an Example Edge List
#'
#' @template param-directed
#' @template param-loops
#'
#' @template author-bk
#'
#' @examples
#' example_el()
#'
#' example_el(directed = TRUE)
#'
#' example_el(loops = TRUE)
#'
#' example_el(directed = TRUE, loops = TRUE)
#'
#' @export
example_el <- function(directed = FALSE, loops = FALSE) {
  if (directed && !loops) {
    structure(c(3, 5, 5, 1, 2, 4, 5, 1, 2, 3, 1, 3, 1, 1, 2, 3, 3,
                3, 3, 4, 4, 4, 5, 5), .Dim = c(12L, 2L))
  } else if (directed && loops) {
    structure(c(1, 3, 5, 2, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 1, 3, 5,
                1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5),
              .Dim = c(17L, 2L))
  } else if (!directed && !loops) {
    structure(c(1, 1, 3, 3, 3, 5, 4, 5), .Dim = c(4L, 2L))
  } else if (!directed && loops) {
    structure(c(1, 1, 1, 2, 3, 3, 3, 4, 5, 1, 3, 5, 2, 3, 4, 5, 4,
                5), .Dim = c(9L, 2L))
  }
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
#' @importFrom igraph ecount edge_attr<- graph_from_adjacency_matrix
#' @importFrom igraph graph_from_edgelist vertex_attr<-
#' @export
example_igraph <- function(directed = FALSE, loops = FALSE,
                           initial_rep = c("adj_mat", "el")) {
  initial_rep <- match.arg(initial_rep, c("adj_mat", "el"))

  out <- switch (initial_rep,
    "adj_mat" = graph_from_adjacency_matrix(
                  adjmatrix = example_adj_mat(directed, loops),
                  mode = if (directed) "directed" else "undirected"
                )
    ,
    "el" = graph_from_edgelist(
            el = example_el(directed = directed, loops = loops),
            directed = directed
            )
    ,
    stop("Unknown `initial_rep`.", call. = FALSE)
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
example_network <- function(directed = FALSE, loops = FALSE,
                            initial_rep = c("adj_mat", "el")) {
  initial_rep = match.arg(initial_rep, c("adj_mat", "el"))

  out <- switch (initial_rep,
    # "adj_mat" = as.network.matrix(
    #               x = example_adj_mat(directed, loops),
    #               matrix.type = "adjacency",
    #               directed = directed,
    #               loops = loops,
    #               hyper = FALSE,
    #               multiple = FALSE
    #             )
    "adj_mat" = adj_mat_as_network(
                  x = example_adj_mat(directed, loops),
                  directed = directed
                )
    ,
    "el" = as.network.matrix(
            x = example_adj_mat(directed, loops),
            # TODO why does matrix.type="edgelist" fail with ...
                    # Error in add.edges.network(g, as.list(x[, 1]), as.list(x[, 2]), edge.check = edge.check) :
                      # (edge check) Illegal vertex reference in addEdges_R.  Exiting.
            # matrix.type = "edgelist",
            directed = directed,
            loops = loops,
            hyper = FALSE,
            multiple = FALSE
          )
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




