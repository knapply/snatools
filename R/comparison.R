have_same_edgelists <- function(x, y) {
  x_el <- get_el(x)
  y_el <- get_el(y)
  if (inherits(x, "network") && !x[["gal"]][["directed"]]) {
    x_el <- sort_el_cols_by_row(x_el)
  }
  y_el <- get_el(y)
  if (inherits(x, "network") && !x[["gal"]][["directed"]]) {
    y_el <- sort_el_cols_by_row(y_el)
  }
  if (all(dim(x_el) == dim(y_el))) {
    return(identical(x_el, y_el))
  }
  FALSE
}

have_same_edge_attrs <- function(x, y) {
  x_attrs <- edg_as_df(x)
  x_attrs[, c(".ego", ".alter")] <- NULL
  y_attrs <- edg_as_df(y)
  y_attrs[, c(".ego", ".alter")] <- NULL
  if (all(dim(x_attrs) == dim(y_attrs))) {
    return(identical(x_attrs, y_attrs))
  }
  FALSE
}

have_same_vert_attrs <- function(x, y) {
  x_attrs <- vrt_as_df(x)
  y_attrs <- vrt_as_df(y)
  if (all(dim(x_attrs) == dim(y_attrs))) {
    return(identical(x_attrs, y_attrs))
  }
  FALSE
}

have_same_metadata <- function(x, y) {
  all(net_is_bipartite(x) == net_is_bipartite(y),
      net_is_directed(x) == net_is_directed(y))
}


are_same_graphs <- function(x, y) {
  # validate_args(x = x, validate_graph = TRUE)
  # validate_args(x = y, validate_graph = TRUE)
  if (inherits(x, "igraph") && net_is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
  }
  if (inherits(y, "igraph") && net_is_bipartite(y)) {
    y <- prep_bipartite_igraph(y)
  }
  tests <-c(have_same_metadata = have_same_metadata(x, y),
            have_same_edgelists = have_same_edgelists(x, y),
            have_same_edge_attrs = have_same_edge_attrs(x, y),
            have_same_vert_attrs = have_same_vert_attrs(x, y))
  if (all(tests)) {
    return(TRUE)
  }
  fails <- paste0(names(tests)[!tests], collapse = ", ")
  glue_message("The following tests failed:\n{fails}")
  FALSE
}

# vrt_as_df(ig_bip)
# vrt_as_df(as_network(ig_bip))
# x <- ig_bip
# y <- as_network(ig_bip)
# have_same_vert_attrs(ig_bip, as_network(ig_bip))
# are_same_graphs(x, y)
