#' Extract a graph objects's vertex attribute names.
#' 
#' @param x An `sna_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return `character` `vector` listing the names of `x`'s vertex attributes.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [vrt_get_attr()], [vrt_get_attr_df()], [igraph::vertex_attr_names()],
#' [network::list.vertex.attributes()]
#' 
#' @examples 
#' library(snatools)
#' 
#' adj_mat <- rbinom(16L, 1L, 0.3) %>% 
#'   matrix(nrow = 4L, dimnames = list(letters[1:4], letters[1:4]))
#'
#' ig <- adj_mat %>% 
#'   igraph::graph_from_adjacency_matrix() %>% 
#'   igraph::set_vertex_attr("another_attr", value = sample(seq_len(igraph::vcount(.)))) 
#' 
#' vrt_attr_names(ig)
#' 
#' nw <- network::as.network(adj_mat)
#' network::set.vertex.attribute(nw, "another_attr", value = sample(seq_len(network::network.size(nw))))
#' 
#' vrt_attr_names(nw)
#' 
#' tbl_g <- tidygraph::as_tbl_graph(ig)
#' 
#' vrt_attr_names(tbl_g)
#' 
#' sna_g <- as_sna_net(ig)
#' 
#' vrt_attr_names(sna_g)
#' 
#' @export
vrt_attr_names <- function(x) {
  UseMethod("vrt_attr_names")
}

#' @rdname vrt_attr_names
#' @export
vrt_attr_names.sna_net <- function(x) {
  names(x[["vertices"]])
}

#' @rdname vrt_attr_names
#' @importFrom igraph vertex_attr_names
#' @export
vrt_attr_names.igraph <- function(x) {
  out <- vertex_attr_names(x)
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname vrt_attr_names
#' @export
vrt_attr_names.network <- function(x) {
  out <- unique(unlist(lapply(x[["val"]], names)))
  out <- out[!out == "na"]
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname vrt_attr_names
#' @export
vrt_attr_names.tbl_graph <- function(x) {
  vrt_attr_names(as_igraph(x))
}




#' Extract a specific vertex attribute.
#' 
#' @param x An `sna_net`, `igraph`, `network`, or `tbl_graph`.
#' @param vrt_attr `character` specifying the target vertex attribute.
#' 
#' @return `vector` of vertex attribute values.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [vrt_attr_names()], [vrt_get_attr_df()], [igraph::vertex_attr()],
#' [network::get.vertex.attribute()]
#' 
#' @examples 
#' library(snatools)
#' 
#' adj_mat <- rbinom(16L, 1L, 0.3) %>% 
#'   matrix(nrow = 4L, dimnames = list(letters[1:4], letters[1:4]))
#'
#' ig <- adj_mat %>% 
#'   igraph::graph_from_adjacency_matrix() %>% 
#'   igraph::set_vertex_attr("a_vertex_attribute", value = sample(seq_len(igraph::vcount(.)))) 
#' 
#' ig %>% 
#'   vrt_get_attr("name")
#' 
#' nw <- network::as.network(adj_mat)
#'   network::set.vertex.attribute(nw, "a_vertex_attribute", value = sample(seq_len(network::network.size(nw))))
#' 
#' nw %>% 
#'   vrt_get_attr("vertex.names")
#' 
#' tbl_g <- tidygraph::as_tbl_graph(ig)
#' 
#' tbl_g %>% 
#'   vrt_get_attr("a_vertex_attribute")
#' 
#' sna_g <- as_sna_net(nw)
#' 
#' vrt_get_attr(sna_g, "a_vertex_attribute")
#' 
#' @export
vrt_get_attr <- function(x, vrt_attr) {
  UseMethod("vrt_get_attr")
}

#' @rdname vrt_get_attr
#' 
#' @export
vrt_get_attr.sna_net <- function(x, vrt_attr) {
  x[["vertices"]][[vrt_attr]]
}

#' @rdname vrt_get_attr
#' 
#' @importFrom igraph vertex_attr
#' @export
vrt_get_attr.igraph <- function(x, vrt_attr) {
  vertex_attr(x, vrt_attr)
}

#' @rdname vrt_get_attr
#' 
#' @export
vrt_get_attr.network <- function(x, vrt_attr) {
  unlist(lapply(x[["val"]], function(x) {
    x[[vrt_attr]] %||% NA
    }))
}

#' @rdname vrt_get_attr
#' 
#' @export
vrt_get_attr.tbl_graph <- function(x, vrt_attr) {
  vrt_get_attr(as_igraph(x), vrt_attr)
}


#' Extract all vertex attributes as a data frame.
#' 
#' @param x An `sna_net`, `igraph`, `network`, or `tbl_graph`.
#' @param sna_vertices_df `logical` (default: `TRUE`) indicating whether to return
#' an `sna_vertices_df` object that standardizes the results of graph objects.
#' 
#' @return `data.frame` or `sna_vertices_df` with all vertex attributes contained in `x`.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [vrt_get_attr()], [vrt_attr_names()], [igraph::as_data_frame()]
#' 
#' @examples 
#' library(snatools)
#' 
#' adj_mat <- rbinom(16L, 1L, 0.3) %>% 
#'   matrix(nrow = 4L, dimnames = list(letters[1:4], letters[1:4]))
#'
#' ig <- adj_mat %>% 
#'   igraph::graph_from_adjacency_matrix() %>% 
#'   igraph::set_vertex_attr("a_vertex_attribute", value = sample(seq_len(igraph::vcount(.)))) 
#' 
#' vrt_get_attr_df(ig)
#' vrt_get_attr_df(ig, sna_vertices_df = FALSE)
#' 
#' nw <- network::as.network(adj_mat)
#' network::set.vertex.attribute(nw, "a_vertex_attribute", value = sample(seq_len(network::network.size(nw))))
#' 
#' vrt_get_attr_df(nw)
#' vrt_get_attr_df(nw, sna_vertices_df = FALSE)
#' 
#' tbl_g <- tidygraph::as_tbl_graph(ig)
#' 
#' vrt_get_attr_df(tbl_g)
#' vrt_get_attr_df(tbl_g, sna_vertices_df = FALSE)
#' 
#' sna_g <- as_sna_net(nw)
#' 
#' vrt_get_attr_df(sna_g)
#' 
#' @export
vrt_get_attr_df <- function(x, sna_vertices_df = TRUE) {
  UseMethod("vrt_get_attr_df")
}

#' @rdname vrt_get_attr_df
#' 
#' @export
vrt_get_attr_df.sna_net <- function(x) {
  x[["vertices"]]
}

#' @rdname vrt_get_attr_df
#' 
#' @importFrom igraph as_data_frame set_vertex_attr V vcount vertex_attr
#' @export
vrt_get_attr_df.igraph <- function(x, sna_vertices_df = TRUE) {
  if (net_count_vertices(x) == 0L) {
    return(NULL)
  }
  if (!"name" %in% vrt_attr_names(x)) {
    x <- set_vertex_attr(x, "name", value = seq_len(vcount(x)))
  }
  if (sna_vertices_df && net_is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
  }
  out <- as_data_frame(x, what = "vertices")
  if (sna_vertices_df) {
    if (net_is_bipartite(x)) {
      out[[".actor"]] <- out[["type"]]
      out[["type"]] <- NULL
    }
    return(as_sna_vertices_df(out))
  }
  out
}

#' @rdname vrt_get_attr_df
#' 
#' @export
vrt_get_attr_df.network <- function(x, sna_vertices_df = TRUE) {
  if (net_count_vertices(x) == 0L) {
    return(NULL)
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    out <- data.table::rbindlist(x[["val"]], fill = TRUE)
    out <- as.data.frame(out)
  } else if (requireNamespace("dplyr", quietly = TRUE)) {
    out <- dplyr::bind_rows(x[["val"]])
  } else {
    out <- tryCatch(
      do.call(rbind.data.frame, c(x[["val"]], stringsAsFactors = FALSE, fill = NA)),
      error = function(e) "some vertices missing attributes"
      )
    if (out != "some vertices missing attributes") {
      return(out)
    }
    col_names <- vrt_attr_names(x)
    out <- matrix(ncol = length(col_names), dimnames = list(NULL, col_names))
    out <- as.data.frame(out, stringsAsFactors = FALSE)
    attr_lists <- lapply(x[["val"]], `[`)
    attr_lists <- lapply(attr_lists, function(x) {
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
  if (sna_vertices_df) {
    if (net_is_bipartite(x)) {
      out[[".actor"]] <- c(rep(TRUE, x[["gal"]][["bipartite"]]),
                           rep(FALSE, x[["gal"]][["n"]] - x[["gal"]][["bipartite"]]))
    }
    return(as_sna_vertices_df(out))
  }
  out
}

#' @rdname vrt_get_attr_df
#' 
#' @export
vrt_get_attr_df.tbl_graph <- function(x, sna_vertices_df = TRUE) {
  vrt_get_attr_df(as_igraph(x), sna_vertices_df)
}


as_sna_vertices_df <- function(x) {
  if (!is.data.frame(x)) {
    terminate("`x` must be a `data.frame`")
  }
  if (class(x)[[1]] != "data.frame") {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  if ("vertex.names" %in% colnames(x)) {
    colnames(x)[colnames(x) == "vertex.names"] <- ".name"
  } else if ("name" %in% colnames(x)) {
    colnames(x)[colnames(x) == "name"] <- ".name"
  } else {
    x <- x[[".name"]] <- seq_len(nrow(x))
  }
  if (".actor" %in% colnames(x)) {
    other_cols <- colnames(x)[!colnames(x) %in% c(".name", ".actor")]
    x <- cbind.data.frame(x[, ".name", drop = FALSE],
                          x[, ".actor", drop = FALSE],
                          x[, other_cols, drop = FALSE],
                          stringsAsFactors = FALSE)
  } else {
    other_cols <- colnames(x)[colnames(x) != ".name"]
    x <- cbind.data.frame(x[, ".name", drop = FALSE],
                          x[, other_cols, drop = FALSE],
                          stringsAsFactors = FALSE)
  }

  rownames(x) <- NULL
  class(x) <- c("sna_vertices_df", "data.frame")
  x
}

print.sna_vertices_df <- function(x, .nrow = 10L, .verbose = TRUE) {
  out <- head(x, .nrow)
  rownames(out) <- paste0("  ", seq_len(nrow(out)))
  if (.verbose) {
    cat_patch("# sna_vertices_df: %s x %s",
              format(nrow(x), big.mark = ","),
              format(ncol(x), big.mark = ","))
    cat("\n")
  } else {
    cat_patch("$vertices # A %s:  %s x %s ",
              class(x)[[2]],
              format(nrow(x), big.mark = ","),
              format(ncol(x), big.mark = ","))
    cat("\n")
  }
  print.data.frame(out)
  if (nrow(out) < nrow(x)) {
    cat("  ")
    cat_patch("  # ... with %s additional rows.", nrow(x) - nrow(out))
  }
}



