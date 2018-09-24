#' A data structure that effectively bridges `igraph` and `network` objects.
#' 
#' @param x `igraph` ([`igraph::graph`]), [`network::network`], or
#' [`tidygraph::tbl_graph`].
#' 
#' @export
as_bridge_net <- function(x) {
  UseMethod("as_bridge_net")
}

#' @rdname as_bridge_net
#' @export
as_bridge_net.bridge_net <- function(x) {
  x
}

#' @rdname as_bridge_net
#' 
#' @importFrom igraph is_bipartite
#' @export
as_bridge_net.igraph <- function(x) {
  if (is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
  }
  out <- list(metadata = get_metadata(x),
              net_attrs = net_attrs_to_list(x),
              edges = cbind(rep_as_edgelist(x, use_names = FALSE), 
                            edg_to_df(x, include_dyad = FALSE, leave_raw = TRUE),
                            stringsAsFactors = FALSE),
              vertices = vrt_to_df(x, leave_raw = FALSE))
  class(out) <- "bridge_net"
  out
}

#' @rdname as_bridge_net
#' @export
as_bridge_net.network <- function(x) {
  out <- list(metadata = get_metadata(x),
              net_attrs = net_attrs_to_list(x),
              edges = cbind(rep_as_edgelist(x, use_names = FALSE), 
                            edg_to_df(x, include_dyad = FALSE, leave_raw = TRUE),
                            stringsAsFactors = FALSE),
              vertices = vrt_to_df(x))
  class(out) <- "bridge_net"
  out
}

#' @rdname as_bridge_net
#' 
#' @export
as_bridge_net.tbl_graph <- function(x) {
  as_bridge_net.igraph(as_igraph.tbl_graph(x))
}

#' @rdname as_bridge_net
#' 
#' @export
print.bridge_net <- function(x, .nrow = 3L) {
  dir_chr <- ifelse(x[["metadata"]][["is_directed"]],
                    "directed", "undirected")
  article <- ifelse(dir_chr == "directed", "A", "An")
  multi_chr <- ifelse(x[["metadata"]][["is_multiplex"]],
                      "multiplex,", "")
  bip_chr <- ifelse(x[["metadata"]][["is_bipartite"]],
                    "bipartite", "1-mode")
  
  descrip <- patch("%s %s, %s %s `bridge_net`.", 
                     article, dir_chr, multi_chr, bip_chr)
  cat_patch("# %s %s, %s %s `bridge_net`.", article, dir_chr, multi_chr, bip_chr)
  cat("\n")
  loops_chr <- ifelse(x[["metadata"]][["has_loops"]], ">0", 0L)
  iso_chr <- ifelse(x[["metadata"]][["has_isolates"]], ">0", 0L)
  loops_iso <- sprintf("- Contains %s loops and %s isolates.", loops_chr, iso_chr)
  cat_patch("# - Contains %s loops and %s isolates.", loops_chr, iso_chr)
  cat("\n")
  if (!is.null(x[["edges"]]) && nrow(x[["edges"]])) {
    edges <- head(x[["edges"]], .nrow)
    if (!is.null(x[["vertices"]])) {
      if (is.matrix(edges)) {
        edges <- as.data.frame(edges)
        edges[, ".ego"] <- x[["vertices"]][[".name"]][edges[, ".ego"]]
        edges[, ".alter"] <- x[["vertices"]][[".name"]][edges[, ".alter"]]
        edges <- as.matrix(edges)
      } else if (is.data.frame(edges)) {
        edges[, ".ego"] <- x[["vertices"]][[".name"]][edges[, ".ego"]]
        edges[, ".alter"] <- x[["vertices"]][[".name"]][edges[, ".alter"]]
      }
    }
    nrow_edges <- nrow(edges)
    cat(sprintf("$edges # first %s of %s", 
                nrow_edges, 
                format(x[["metadata"]][["n_edges"]], big.mark = ",")))
    cat("\n")
    if (!is.null(x[["vertices"]])) {
      print(format_table(edges, edges_or_verts = "edges", 
                         specify_edge_dyad_types = TRUE), 
            quote = FALSE)
    } else {
      print(format_table(edges, edges_or_verts = "edges", 
                         specify_edge_dyad_types = TRUE), 
            quote = FALSE) 
    }
  } else {
    cat("# No edge data.")
    cat("\n")
  }
  if (!is.null(x[["vertices"]]) && nrow(x[["vertices"]])) {
    vertices <- head(x[["vertices"]], .nrow)
    nrow_verts <- nrow(vertices)
    cat(sprintf("$vertices # first %s of %s", 
                nrow_verts, 
                format(x[["metadata"]][["n_vertices"]], big.mark = ",")))
    cat("\n")
    print(format_table(vertices, edges_or_verts = "verts"), quote = FALSE)
  } else {
    cat("# No vertex data.")
  }
  invisible(x)
}


