#' Build a network/graph's mixing matrix.
#' 
#' @param x An `igraph` or `network` object.
#' @param vrt_attr `character` representing a vertex attribute.
#' 
#' 
#' @export
#' 
mix_mixing_matrix <- function(x, vrt_attr, drop_loops = FALSE) {
  UseMethod("mix_mixing_matrix")
}

#' @rdname mix_mixing_matrix
#' 
#' @export
#' 
mix_mixing_matrix.igraph <- function(x, vrt_attr, drop_loops = FALSE) {
  if(!is.character(vrt_attr)) {
    stop("`vrt_attr` must be a `character`.", call. = FALSE)
  }
  if(!vrt_attr %in% igraph::vertex_attr_names(x)) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  if(drop_loops) {
    x <- igraph::simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
  }
  attrs <- igraph::vertex_attr(x, vrt_attr)
  cats <- sort(unique(attrs))
  el <- igraph::as_edgelist(x, names = FALSE)
  el_list <- list(el[, 1], el[, 2])
  init <- lapply(el_list, function(x) {
    factor(attrs[x], levels = cats)
  })
  table(from = init[[1]], to = init[[2]])
}

#' @rdname mix_mixing_matrix
#' 
#' @export
#' 
mix_mixing_matrix.network <- function(x, vrt_attr, drop_loops = FALSE) {
  if(!is.character(vrt_attr)) {
    stop("`vrt_attr` must be a `character`.", call. = FALSE)
  }
  if(!vrt_attr %in% unlist(lapply(x$val, names))) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  if(drop_loops) {
    network::set.network.attribute(x, "loops", value = FALSE)
  }
  attrs <- network::get.vertex.attribute(x, vrt_attr)
  cats <- sort(unique(attrs))
  el <- network::as.matrix.network.edgelist(x)
  if(nrow(el) == 0L) {
    message("`x` does not have any edges.")
  }
  from <- factor(attrs[el[, 1]], levels = cats)
  to <- factor(attrs[el[, 2]], levels = cats)
  
  table(from, to)
}



#' E-I Index
#' 
#' The global ratio of external to internal ties.
#' 
#' \deqn{E-I~Index = \frac{EL-IL}{EL+IL}}
#' * \eqn{EL} = the number of external ties.
#' * \eqn{IL} = the number of inteneral ties.
#' 
#' @param x An `igraph` or `network` object.
#' @param vrt_attr `character` representing the name of a categorical vertex attribute present in `x`.
#' 
#' @return `numeric` of length 1,  with `-1` suggesting homophilous tendencies and `+1` suggesting heterophilous tendencies.
#' 
#' @references Krackhardt, David, and Robert N. Stern. "Informal Networks and 
#' Organizational Crises: An Experimental Simulation." Social Psychology Quarterly 51, no.
#'  2 (1988): 123-40. \url{http://www.jstor.org/stable/2786835}.
#'  
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples
#' library(snatools)
#' 
#' # igraph ==========================================================================
#' data("UKfaculty", package = "igraphdata")
#' 
#' UKfaculty %>% 
#'   mix_ei_index(vrt_attr = "Group")
#' 
#' # network =========================================================================
#' data("sampson", package = "ergm")
#' 
#' samplike %>% 
#'   mix_ei_index("group")
#' 
#' @export
#' 
mix_ei_index <- function(x, vrt_attr, drop_loops = FALSE) {
  UseMethod("mix_ei_index")
}


#' @rdname mix_ei_index
#' 
#' @export
#' 
mix_ei_index.igraph <- function(x, vrt_attr, drop_loops = FALSE) {
  if(!is.character(vrt_attr)) {
    stop("`vrt_attr` must be a `character`.", call. = FALSE)
  }
  if(drop_loops) {
    x <- igraph::simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
  }
  attrs <- igraph::vertex_attr(x, vrt_attr)
  el <- igraph::as_edgelist(x, names = FALSE)
  
  attr_el <- matrix(attrs[el], ncol = 2)
  
  class(attr_el) <- "attr_el"
  
  mix_ei_index(attr_el)
}

#' @rdname mix_ei_index
#' 
#' @export
#' 
mix_ei_index.network <- function(x, vrt_attr, drop_loops = FALSE) {
  if(!is.character(vrt_attr)) {
    stop("`vrt_attr` must be a `character`.", call. = FALSE)
  }
  if(!vrt_attr %in% unlist(lapply(x$val, names))) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  if(drop_loops) {
    network::set.network.attribute(x, "loops", value = FALSE)
  }
  attrs <- network::get.vertex.attribute(x, vrt_attr)
  el <- network::as.matrix.network.edgelist(x)
  
  attr_el <- matrix(attrs[el], ncol = 2)
  
  class(attr_el) <- "attr_el"
  
  mix_ei_index(attr_el)
}

#' @rdname mix_ei_index
#' 
#' @export
#' 
mix_ei_index.attr_el <- function(attr_el) {
  n <- nrow(attr_el)
  internal <- length(which(attr_el[, 1] == attr_el[, 2]))
  external <- n - internal
  
  (external - internal) / n
}
