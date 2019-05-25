
fetch_edgelist <- function(.net, .node_names = TRUE, .node_attr = NULL, ...) {
  stopifnot(is.logical(.node_names))

  if (!is.null(.node_attr)) {
    fill <- node_get_attr(.net, .node_attr)
  } else if (.node_names) {
    fill <- node_get_names(.net)
  } else {
    fill <- NULL
  }

  out <- .fetch_edgelist(.net)

  if (is.null(fill)) {
    return(out)
  }

  matrix(fill[out], ncol = 2L)
}



.fetch_edgelist <- function(.net, .weighted = FALSE) {
  UseMethod(".fetch_edgelist")
}

#' @importFrom igraph as_edgelist
.fetch_edgelist.igraph <- function(.net, .weighted = FALSE) {
  as_edgelist(graph = .net, names = FALSE)
}

.fetch_edgelist.network <- function(.net, .weighted = FALSE) {
  out <- network::as.matrix.network.edgelist(.net)
  attr(out, "n") <- NULL
  attr(out, "vnames") <- NULL

  out
}

