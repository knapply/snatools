ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex"),
                     permute = FALSE) {
  scope <- match.arg(scope, c("global", "groups", "vertices"))
  switch(scope,
         global = ei_index_global(x, vrt_attr, permute),
         group = ei_index_group(x, vrt_attr),
         vertex = ei_index_vertex(x, vrt))
}

ei_index_global <- function(x, vrt_attr) {
  if (!class(x)[[1]] %in% c("sna_net", "igraph", "network", "tbl_graph")) {
    stop("`x` must be an `sna_net`, `igraph`, `network`, or `tbl_graph`", call. = FALSE)
  }
  if (vrt_attr == ".name") {
    stop('".name" is not a valid vertex attribute.', call. = FALSE)
  }
  el <- rep_as_edgelist(x, vrt_attr)
  n_edges <- nrow(el)
  external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
  internal <- n_edges - external
  
  (external - internal) / n_edges
}


