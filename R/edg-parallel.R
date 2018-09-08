#' Parallel Edges Queries
#' 
#' @rdname parallel-edges
#' 
#' @param x A graph object.
#' @param ... Arguments passed on to other methods. See below.
#' 
#' @return A `logical` or `numeric` scalar or [`tibble::tibble`].
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples
#' library(snatools)
#' 
#' data("USairports", package = "igraphdata")
#' (ig <- USairports)
#' 
#' data("sampson", package = "ergm")
#' (nw <- samplike)
#' 
#' edg_any_parallel(ig)
#' edg_any_parallel(nw)
#' 
#' nw_undirected <- network::network.copy(nw)
#' network::set.network.attribute(nw_undirected, "directed", FALSE)
#' 
#' network::is.multiplex(nw_undirected)
#' edg_any_parallel(nw_undirected)
#' 
#' edg_sum_parallel(ig)
#' length(which(igraph::which_multiple(ig)))
#' 
#' edg_sum_parallel(nw_undirected)
#' 
#' edg_count_parallel(ig) %>% head(10)
#' edg_count_parallel(nw) %>% head(10)
#' 
#' edg_count_parallel_df(USairports)
#' edg_count_parallel_df(nw)
#' 

#' @rdname parallel-edges
#' 
#' @details `edg_any_parallel()` checks whether `x` contains parallel edges and returns a
#' `logical` scalar. \cr
#' * In directed graphs, edges are considered parallel if they share the same source and target.
#' * In undirected graphs, edges are considered parallel if they share the same two vertices.
#' * `edg_any_parallel()` differs from [network::is.multiplex()], which checks if a 
#' `network::network` is _allowed_ to contain _multiplex_ edges, not whether any parallel
#' edges exist.
#' 
#' @seealso [`igraph::any_multiple()`], [`network::is.multiplex()`]
#'
#' @export
#' 
edg_any_parallel <- function(x) {
  el <- rep_edgelist(x)
  if(net_is_directed(x)) {
    return(nrow(el) != nrow(unique(el)))
  }
  el2 <- cbind(el[, 2], el[, 1])
  full <- rbind(el, el2)
  
  nrow(full) != nrow(unique(full))
}

#' @rdname parallel-edges
#' 
#' @details `edg_sum_parallel()` counts the _total_ number of parallel edges and returns an `integer` scalar. \cr
#' 
#' @seealso [`igraph::count_multiple()`], [`igraph::which_multiple()`]
#'
#' @export
#' 
edg_sum_parallel <- function(x) {
  el <- rep_edgelist(x)
  if(net_is_directed(x)) {
    return(nrow(el) - nrow(unique(el)))
  }
  el2 <- cbind(el[, 2], el[, 1])
  full <- rbind(el, el2)
  
  nrow(full) - nrow(unique(full))
}

#' @rdname parallel-edges
#' 
#' @details `edg_count_parallel()` counts the number of parallel edges for _each edge_ and 
#' returns an `integer` `vector`. \cr
#' * `edg_count_parallel()` differs from [`igraph::count_multiple()`], which returns fractions 
#' for loops.
#' 
#' @seealso [`igraph::count_multiple()`]
#'
#' @export
#' 
edg_count_parallel <- function(x) {
  el <- rep_edgelist(x)
  if(net_is_directed(x)) {
    dyads <- apply(el, 1, function(x) {
      paste0(x, collapse = ",")
    })
  } else {
    dyads <- apply(el, 1, function(x) {
      paste0(sort(x), collapse = ",")
    })
  }
  dyad_df <- data.frame(dyad = dyads, stringsAsFactors = FALSE)
  
  unsplit(lapply(split(dyad_df, dyad_df[, "dyad"]), nrow), dyad_df["dyad"])
}

#' @rdname parallel-edges
#' 
#' @details 
#' * `edg_count_parallel_df` is a convenience wrapper around `edg_count_parallel()` and
#' `edg_get_attrs()` that returns a `tibble::tibble`.
#' 
#' @export
#' 
edg_count_parallel_df <- function(x, ...) {
  parallel <- edg_count_parallel(x)
  out <- edg_get_attrs(x, include_el = TRUE, ...)
  out$n <- parallel
  
  out
}