have_same_edgelists <- function(x, y) {
  x_el <- get_el(x)
  y_el <- get_el(y)
  if (inherits(x, "network") && !x[["gal"]][["directed"]]) {
    x_el <- sort_el_cols_by_row(x_el)
  }
  y_el <- get_el(y)
  if (inherits(y, "network") && !y[["gal"]][["directed"]]) {
    y_el <- sort_el_cols_by_row(y_el)
  }
  if (all(dim(x_el) == dim(y_el))) {
    return(identical(x_el, y_el))
  }
  FALSE
}

have_same_edge_attrs <- function(x, y) {
  x_attrs <- as.data.frame(x)
  x_attrs[, c(".ego", ".alter")] <- NULL
  y_attrs <- as.data.frame(y)
  y_attrs[, c(".ego", ".alter")] <- NULL
  if (all(dim(x_attrs) == dim(y_attrs))) {
    return(identical(x_attrs, y_attrs))
  }
  FALSE
}

have_same_vertices <- function(x, y) {
  identical(as.data.frame(x, .unit = "vertices"), 
            as.data.frame(y, .unit = "vertices"))
}

have_same_metadata <- function(x, y) {
  all(net_is_bipartite(x) == net_is_bipartite(y),
      net_is_directed(x) == net_is_directed(y))
}

are_same_graphs <- function(x, y) {
  if (inherits(x, "igraph") && net_is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
  }
  if (inherits(y, "igraph") && net_is_bipartite(y)) {
    y <- prep_bipartite_igraph(y)
  }
  tests <- c(have_same_metadata = have_same_metadata(x, y),
             have_same_edgelists = have_same_edgelists(x, y),
             have_same_edge_attrs = have_same_edge_attrs(x, y),
             have_same_vertices = have_same_vertices(x, y))
  if (all(tests)) {
    return(TRUE)
  }
  fails <- paste0(names(tests)[!tests], collapse = ", ")
  glue_message("The following tests failed:\n{fails}")
  FALSE
}

# data("sampson", package = "ergm")
# tibble::as_tibble(samplike)
# g <- network::network.initialize(n = 0)
# as.data.frame(g)
# as.data.frame(g, .unit = "vertices")
# have_same_edgelists(iso_envir1$windsurfers, as_igraph(iso_envir1$windsurfers))
# are_same_graphs(iso_envir1$windsurfers, as_igraph(iso_envir1$windsurfers))
# all.equal(
# head(as.data.frame(iso_envir1$windsurfers)),
# head(as.data.frame(as_igraph(iso_envir1$windsurfers)))
# )
