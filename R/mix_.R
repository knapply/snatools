#' Build a network/graph's mixing matrix.
#' 
#' @param x An `igraph` or `network` object.
#' @param vrt_attr `character` representing a vertex attribute.
#' 
#' 
#' @export
#' 
mix_mixing_matrix <- function(x, vrt_attr) {
  UseMethod("mix_mixing_matrix")
}

#' @rdname mix_mixing_matrix
#' 
#' @export
#' 
mix_mixing_matrix.igraph <- function(x, vrt_attr) {
  if(!is.character(vrt_attr)) {
    stop("`vrt_attr` must be a `character`.", call. = FALSE)
  }
  if(!vrt_attr %in% igraph::vertex_attr_names(x)) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  attrs <- igraph::vertex_attr(x, vrt_attr)
  cats <- sort(unique(attrs))
  el <- igraph::as_edgelist(x, names = FALSE)
  el_list <- list(el[, 1], el[, 2])
  init <- lapply(el_list, function(x) {
    factor(attrs[x], levels = cats)
  })
  out <- table(ego = init[[1]], alter = init [[2]])
  if(igraph::is_directed(x)) {
    return(out)
  }
  alt <- out + t(out)
  diag(alt) <- diag(out)
  alt[lower.tri(alt)] <- 0
  alt
}

#' @rdname mix_mixing_matrix
#' 
#' @export
#' 
mix_mixing_matrix.network <- function(x, vrt_attr) {
  if(!is.character(vrt_attr)) {
    stop("`vrt_attr` must be a `character`.", call. = FALSE)
  }
  if(!vrt_attr %in% unlist(lapply(x$val, names))) {
    stop("`vrt_attr` is not a vertex attribute in `x`", 
         call. = FALSE)
  }
  attrs <- network::get.vertex.attribute(x, vrt_attr)
  cats <- sort(unique(attrs))
  el <- network::as.matrix.network.edgelist(x)
  from <- factor( attrs[el[, 1] ], levels = cats)
  to <- factor(attrs[el[, 2] ], levels = cats)
  out <- table(from, to)
  if(x$gal$directed) {
    return(out)
  }
  alt <- out + t(out)
  diag(alt) <- diag(out)
  alt[lower.tri(alt)] <- 0
  alt
}

#' Krackhardt's E-I Index
#' 
#' @param x An `igraph` or `network` object.
#' @param vrt_attr `character` representing a vertex attribute
#' 
#' @export
#' 
mix_ei_index <- function(x, vrt_attr) {
  if(!is.character(vrt_attr)) {
    stop("`vrt_attr` must be a `character`.", call. = FALSE)
  }
  mix_mat <- mix_mixing_matrix(x, vrt_attr)
  init <- mix_mat / sum(mix_mat)
  internal <- sum(diag(init))
  diag(init) <- 0
  external <- sum(init)
  external - internal
}
