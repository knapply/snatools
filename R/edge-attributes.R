#' Extract a graph objects's edge attribute names.
#' 
#' @param x An `sna_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return `character` `vector` listing the names of `x`'s edge attributes.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [edg_get_attr()], [edg_get_attr_df()], [igraph::edge_attr_names()],
#' [network::list.edge.attributes()]
#' 
#' @examples 
#' library(snatools)
#' 
#' adj_mat <- rbinom(16L, 1L, 0.3) %>% 
#'   matrix(nrow = 4L, dimnames = list(letters[1:4], letters[1:4]))
#'
#' ig <- adj_mat %>% 
#'   igraph::graph_from_adjacency_matrix() %>% 
#'   igraph::set_edge_attr("another_attr", value = sample(seq_len(igraph::ecount(.)))) 
#' edg_attr_names(ig)
#' 
#' nw <- network::as.network(adj_mat)
#' network::set.edge.attribute(nw, "another_attr", value = sample(seq_len(network::network.edgecount(nw))))
#' edg_attr_names(nw)
#' 
#' tbl_g <- tidygraph::as_tbl_graph(ig)
#' edg_attr_names(tbl_g)
#' 
#' sna_g <- as_sna_net(ig)
#' edg_attr_names(sna_g)
#' 
#' @export
edg_attr_names <- function(x) {
  UseMethod("edg_attr_names")
}

#' @rdname edg_attr_names
#' @export
edg_attr_names.sna_net <- function(x) {
  out <- colnames(x[["edges"]])
  out[!out %in% c(".ego", ".alter")]
}

#' @rdname edg_attr_names
#' @importFrom igraph edge_attr_names
#' @export
edg_attr_names.igraph <- function(x) {
  out <- edge_attr_names(x)
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname edg_attr_names
#' @export
edg_attr_names.network <- function(x) {
  out <- unique(unlist(lapply(lapply(x[["mel"]], `[[`, "atl"), names)))
  out <- out[out != "na"]
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname edg_attr_names
#' @export
edg_attr_names.tbl_graph <- function(x) {
  edg_attr_names(as_igraph(x))
}




#' Extract a specific edge attribute.
#' 
#' @param x An `sna_net`, `igraph`, `network`, or `tbl_graph`.
#' @param edg_attr `character` specifying the target edge attribute.
#' 
#' @return `vector` of edge attribute values.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [edg_attr_names()], [edg_get_attr_df()], [igraph::edge_attr()],
#' [network::get.edge.attribute()]
#' 
#' @examples 
#' library(snatools)
#' 
#' adj_mat <- rbinom(16L, 1L, 0.3) %>% 
#'   matrix(nrow = 4L, dimnames = list(letters[1:4], letters[1:4]))
#'
#' ig <- adj_mat %>% 
#'   igraph::graph_from_adjacency_matrix() %>% 
#'   igraph::set_edge_attr("an_edge_attribute", value = sample(seq_len(igraph::ecount(.)))) 
#' 
#' ig %>% 
#'   edg_get_attr("an_edge_attribute")
#' 
#' nw <- network::as.network(adj_mat)
#'   network::set.edge.attribute(nw, "an_edge_attribute", value = sample(seq_len(network::network.edgecount(nw))))
#' 
#' nw %>% 
#'   edg_get_attr("an_edge_attribute")
#' 
#' tbl_g <- tidygraph::as_tbl_graph(ig)
#' 
#' tbl_g %>% 
#'   edg_get_attr("an_edge_attribute")
#' 
#' sna_g <- as_sna_net(nw)
#' 
#' edg_get_attr(sna_g, "an_edge_attribute")
#' 
#' @export
edg_get_attr <- function(x, edg_attr) {
  UseMethod("edg_get_attr")
}

#' @rdname edg_get_attr
#' 
#' @export
edg_get_attr.sna_net <- function(x, edg_attr) {
  validate_edg_attr(x, edg_attr)
  x[["edges_attr"]]
}

#' @rdname edg_get_attr
#' 
#' @importFrom igraph edge_attr
#' @export
edg_get_attr.igraph <- function(x, edg_attr) {
  validate_edg_attr(x, edg_attr)
  edge_attr(x, edg_attr)
}

#' @rdname edg_get_attr
#' 
#' @export
edg_get_attr.network <- function(x, edg_attr) {
  validate_edg_attr(x, edg_attr)
  unlist(lapply(lapply(x[["mel"]], `[[`, "atl"), `[[`, edg_attr %||% NA))
}

#' @rdname edg_get_attr
#' 
#' @export
edg_get_attr.tbl_graph <- function(x, edg_attr) {
  edg_get_attr(as_igraph(x), edg_attr)
}


#' Extract all edge attributes as a data frame.
#' 
#' @param x An `sna_net`, `igraph`, `network`, or `tbl_graph`.
#' @param include_dyad `logical` (default: `TRUE`) indicating whether to return the `.ego`
#' and `.alter` of each edge's dyad.
#' @param leave_raw `logical` (default: `FALSE`) indicating whether to return a bare
#' `data.frame` (`stringsAsFactors = FALSE`) instead of an `sna_edges_df`.
#' 
#' @return `sna_edges_df` `data.frame` with all edge attributes contained in `x`.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [edg_get_attr()], [edg_attr_names()], [igraph::as_data_frame()]
#' 
#' @examples 
#' library(snatools)
#' 
#' adj_mat <- rbinom(16L, 1L, 0.3) %>% 
#'   matrix(nrow = 4L, dimnames = list(letters[1:4], letters[1:4]))
#'
#' ig <- adj_mat %>% 
#'   igraph::graph_from_adjacency_matrix() %>% 
#'   igraph::set_edge_attr("an_edge_attribute", value = sample(seq_len(igraph::ecount(.)))) 
#' 
#' edg_get_attr_df(ig)
#' edg_get_attr_df(ig, include_dyad = FALSE)
#' edg_get_attr_df(ig, leave_raw = TRUE)
#' 
#' nw <- network::as.network(adj_mat)
#' network::set.edge.attribute(nw, "an_edge_attribute", value = sample(seq_len(network::network.edgecount(nw))))
#' 
#' edg_get_attr_df(nw)
#' edg_get_attr_df(nw, include_dyad = FALSE)
#' edg_get_attr_df(nw, leave_raw = TRUE)
#' 
#' tbl_g <- tidygraph::as_tbl_graph(ig)
#' 
#' edg_get_attr_df(tbl_g)
#' edg_get_attr_df(tbl_g, include_dyad = FALSE)
#' edg_get_attr_df(tbl_g, leave_raw = TRUE)
#' 
#' sna_g <- as_sna_net(nw)
#' 
#' edg_get_attr_df(sna_g)
#' 
#' @export
edg_get_attr_df <- function(x, include_dyad = TRUE, leave_raw = TRUE) {
  UseMethod("edg_get_attr_df")
}

#' @rdname edg_get_attr_df
#' 
#' @export
edg_get_attr_df.sna_net <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
  if (net_count_vertices(x) == 0L) {
    return(NULL)
  }
  out <- x[["edges"]]
  if (!include_dyad) {
    out[[".ego"]] <- NULL
    out[[".alter"]] <- NULL
  }
  if (leave_raw) {
    out <- as.data.frame(out, stringsAsFactors = FALSE)
  }
  out
}

#' @rdname edg_get_attr_df
#' 
#' @importFrom igraph as_data_frame
#' @export
edg_get_attr_df.igraph <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
  if (net_count_vertices(x) == 0L) {
    return(NULL)
  }
  if (net_is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
  }
  out <- as_data_frame(x, what = "edges")
  if (include_dyad) {
    colnames(out)[colnames(out) == "from"] <- ".ego"
    colnames(out)[colnames(out) == "to"] <- ".alter"
  } else {
    out[["from"]] <- NULL
    out[["to"]] <- NULL
  }
  if (!leave_raw) {
    return(as_sna_edges_df(out))
  }
  out
}

#' @rdname edg_get_attr_df
#' 
#' @export
edg_get_attr_df.network <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
  if (net_count_vertices(x) == 0L) {
    return(NULL)
  }
  init <- lapply(x[["mel"]], `[[`, "atl")
  if (requireNamespace("data.table", quietly = TRUE)) {
    out <- data.table::rbindlist(init, fill = TRUE)
    out <- as.data.frame(out)
  } else if (requireNamespace("dplyr", quietly = TRUE)) {
    out <- dplyr::bind_rows(init)
  } else {
    out <- tryCatch(
      do.call(rbind.data.frame, c(init, stringsAsFactors = FALSE, fill = NA)),
      error = function(e) "some edges missing attributes"
      )
    if (out != "some edges missing attributes") {
      return(out)
    }
    col_names <- edg_attr_names(x)
    out <- matrix(ncol = length(col_names), dimnames = list(NULL, col_names))
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    attr_lists <- lapply(init, function(x) {
      if (all(col_names %in% names(x))) {
        return(x)
      }
      missing_attrs <- col_names[!col_names %in% names(x)]
      for (i in missing_attrs) {
        x[[missing_attrs]] <- NA
      }
      x
    })
    out <- do.call(rbind.data.frame, c(attr_lists, make.row.names = FALSE, 
                                       stringsAsFactors = FALSE))
  }
  out <- out[, colnames(out) != "na", drop = FALSE]
  if (include_dyad) {
    out <- cbind.data.frame(rep_as_edgelist(x, leave_raw = TRUE), out, 
                            stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  if (!leave_raw) {
    return(as_sna_edges_df(out))
  }
  out
}

as_sna_edges_df <- function(x) {
  stopifnot(is.data.frame(x))
  class(x) <- c("sna_edges_df", "data.frame")
  x
}

#' @rdname edg_get_attr_df
#' 
#' @export
edg_get_attr_df.tbl_graph <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
  edg_get_attr_df(as_igraph(x), include_dyad = include_dyad, leave_raw = leave_raw)
}

print.sna_edges_df <- function(x, .nrow = 10L, .verbose = TRUE) {
  out <- head(x, .nrow)
  rownames(out) <- paste0("  ", seq_len(nrow(out)))
  if (.verbose) {
    cat_patch("# sna_edges_df: %s x %s",
              format(nrow(x), big.mark = ","),
              format(ncol(x), big.mark = ","))
    cat("\n")
  } else {
    cat_patch("$edges # A %s:  %s x %s ",
              class(x)[[2]],
              format(nrow(x), big.mark = ","),
              format(ncol(x), big.mark = ","))
    cat("\n")
  }
  print.data.frame(out)
  if (nrow(out) < nrow(x)) {
    cat("  ")
    cat_patch("  # ... with %s additional rows.", 
              format(nrow(x) - nrow(out), big.mark = ','))
  }
}



