#' @export
count_loops <- function(x) {
  if (!class(x) %in% c("igraph", "network", "sna_net", "tbl_graph")) {
    stop("`x` must be an `sna_net`, `igraph`, `network`, or `tbl_graph` object.", 
         call. = FALSE)
  }
  UseMethod("count_loops")
}

#' @export
count_loops.sna_net <- function(x) {
  length(which(x[["edges"]][, "ego"] == x[["edges"]][, "alter"]))
}

#' @export
count_loops.default <- function(x) {
  temp <- as_sna_net(x, edg_attrs = FALSE, vrt_attrs = FALSE, net_attrs = FALSE)
  count_loops(temp)
}

#' @export
count_isolates <- function(x) {
  if (!class(x) %in% c("igraph", "network", "sna_net", "tbl_graph")) {
    stop("`x` must be an `sna_net`, `igraph`, `network`, or `tbl_graph` object.", 
         call. = FALSE)
  }
  UseMethod("count_isolates")
}

#' @export
count_isolates.sna_net <- function(x) {
  verts <- seq_along(x[["vertices"]][, 1])
  edges <- unique(unlist(x[["edges"]][, 1:2], use.names = FALSE))
  length(verts[!verts %in% edges])
}

#' @export
count_isolates.default <- function(x) {
  temp <- as_sna_net(x, edg_attrs = FALSE, vrt_attrs = TRUE, net_attrs = FALSE)
  count_isolates(temp)
}



