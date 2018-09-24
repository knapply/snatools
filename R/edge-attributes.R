#' Extract a graph objects's edge attribute names.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return `character` `vector` listing the names of `x`'s edge attributes.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [edg_get_attr()], [edg_to_df()], [igraph::edge_attr_names()],
#' [network::list.edge.attributes()]
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' @export
edg_attr_names <- function(x) {
  UseMethod("edg_attr_names")
}

#' @rdname edg_attr_names
#' 
#' @examples 
#' edg_attr_names(sampson_monastery)
#' 
#' @export
edg_attr_names.bridge_net <- function(x) {
  out <- colnames(x[["edges"]])
  out[!out %in% c(".ego", ".alter")]
}

#' @rdname edg_attr_names
#' 
#' @seealso [igraph::edge_attr_names()]
#' 
#' @examples 
#' edg_attr_names(ig)
#' 
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
#' 
#' @seealso [network::list.edge.attributes()]
#' 
#' @examples 
#' edg_attr_names(nw)
#' 
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
#' 
#' @examples 
#' edg_attr_names(tidy_g)
#' 
#' @export
edg_attr_names.tbl_graph <- function(x) {
  edg_attr_names(as_igraph(x))
}


#' Extract a specific edge attribute.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param edg_attr `character` specifying the target edge attribute.
#' 
#' @return `vector` of edge attribute values.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [edg_attr_names()], [edg_to_df()], [igraph::edge_attr()],
#' [network::get.edge.attribute()]
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' @export
edg_get_attr <- function(x, edg_attr) {
  UseMethod("edg_get_attr")
}

#' @rdname edg_get_attr
#' 
#' @examples 
#' edg_get_attr(sampson_monastery, "relation") %>% unique()
#' 
#' @export
edg_get_attr.bridge_net <- function(x, edg_attr) {
  validate_edg_attr(x, edg_attr)
  x[["edges"]][[edg_attr]]
}

#' @rdname edg_get_attr
#' 
#' @seealso [igraph::edge_attr()]
#' 
#' @examples 
#' edg_get_attr(ig, "relation") %>% unique()
#' 
#' @importFrom igraph edge_attr
#' @export
edg_get_attr.igraph <- function(x, edg_attr) {
  validate_edg_attr(x, edg_attr)
  edge_attr(x, edg_attr)
}

#' @rdname edg_get_attr
#' 
#' @examples 
#' edg_get_attr(nw, "relation") %>% unique()
#' 
#' @export
edg_get_attr.network <- function(x, edg_attr) {
  validate_edg_attr(x, edg_attr)
  unlist(lapply(lapply(x[["mel"]], `[[`, "atl"), `[[`, edg_attr %||% NA))
}

#' @rdname edg_get_attr
#' 
#' @examples 
#' edg_get_attr(tidy_g, "relation") %>% unique()
#' 
#' @export
edg_get_attr.tbl_graph <- function(x, edg_attr) {
  edg_get_attr(as_igraph(x), edg_attr)
}


#' Extract all edge attributes as a data frame.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param include_dyad `logical` (default: `TRUE`) indicating whether to return the `.ego`
#' and `.alter` of each edge's dyad.
#' @param leave_raw `logical` (default: `FALSE`) indicating whether to return a bare
#' `data.frame` (`stringsAsFactors = FALSE`) instead of an `edge_data_frame`.
#' 
#' @return `edge_data_frame` `data.frame` with all edge attributes contained in `x`.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [edg_get_attr()], [edg_attr_names()], [igraph::as_data_frame()]
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' @export
edg_to_df <- function(x, include_dyad = TRUE, leave_raw = TRUE) {
  UseMethod("edg_to_df")
}

#' @rdname edg_to_df
#' 
#' @examples 
#' edg_to_df(sampson_monastery)
#' 
#' @export
edg_to_df.bridge_net <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
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
  as_edge_data_frame(out)
}

#' @rdname edg_to_df
#' 
#' @seealso [igraph::as_data_frame()]
#' 
#' @examples 
#' edg_to_df(ig)
#' 
#' @importFrom igraph as_data_frame
#' @export
edg_to_df.igraph <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
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
    return(as_edge_data_frame(out))
  }
  out
}

#' @rdname edg_to_df
#' 
#' @examples 
#' edg_to_df(nw)
#' 
#' @export
edg_to_df.network <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
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
    return(as_edge_data_frame(out))
  }
  out
}

as_edge_data_frame <- function(x) {
  stopifnot(is.data.frame(x))
  class(x) <- c("edge_data_frame", "data.frame")
  x
}

#' @rdname edg_to_df
#' 
#' @examples 
#' edg_to_df(tidy_g)
#' 
#' @export
edg_to_df.tbl_graph <- function(x, include_dyad = TRUE, leave_raw = FALSE) {
  edg_to_df(as_igraph(x), include_dyad = include_dyad, leave_raw = leave_raw)
}

#' @rdname edg_to_df
#' 
#' @param .nrow Maximum number of rows to `print()`.
#' 
#' @importFrom utils head
#' @export
print.edge_data_frame <- function(x, .nrow = 10L) {
  out <- head(x, .nrow)
  rownames(out) <- paste0("  ", seq_len(nrow(out)))
  # if (.verbose) {
    cat_patch("# edge_data_frame: %s x %s",
              format(nrow(x), big.mark = ","),
              format(ncol(x), big.mark = ","))
    cat("\n")
  # } else {
  #   cat_patch("$edges # A %s:  %s x %s ",
  #             class(x)[[2]],
  #             format(nrow(x), big.mark = ","),
  #             format(ncol(x), big.mark = ","))
  #   cat("\n")
  # }
  print.data.frame(out)
  if (nrow(out) < nrow(x)) {
    cat("  ")
    cat_patch("  # ... with %s additional rows.", 
              format(nrow(x) - nrow(out), big.mark = ','))
  }
  cat("\n")
  invisible(x)
}

#' Delete Edge Attributes
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param ... `character` including the name(s) of one or more edge attributes to delete 
#' from `x`.
#' 
#' @return `x`, but with `...` deleted.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' @export
edg_delete_attr <- function(x, ...) {
  UseMethod("edg_delete_attr")
}

#' @rdname edg_delete_attr
#' 
#' @examples 
#' sampson_monastery %>% 
#'   edg_delete_attr("relation", "time")
#'   
#' @export
edg_delete_attr.bridge_net <- function(x, ...) {
  attrs <- c(...)
  validate_edg_attr(x, attrs, max_length = Inf)
  x[["edges"]][, attrs] <- NULL
  x
}

#' @rdname edg_delete_attr
#' 
#' @seealso [`igraph::delete_edge_attr()`]
#' 
#' @examples 
#' ig %>% 
#'   edg_delete_attr("time")
#' 
#' @importFrom igraph delete_edge_attr
#' @export
edg_delete_attr.igraph <- function(x, ...) {
  attrs <- c(...)
  validate_edg_attr(x, attrs, max_length = Inf)
  for (i in seq_along(attrs)) {
    x <- igraph::delete_edge_attr(x, attrs[[i]])
  }
  x
}

#' @rdname edg_delete_attr
#' 
#' @seealso [`network::delete.vertex.attribute()`]
#' 
#' @examples 
#' nw %>% 
#'   edg_delete_attr("relation", "weight")
#' 
#' @importFrom network delete.edge.attribute
#' @export
edg_delete_attr.network <- function(x, ...) {
  attrs <- c(...)
  validate_edg_attr(x, attrs, max_length = Inf)
  for (i in seq_along(attrs)) {
    network::delete.edge.attribute(x, attrs[[i]])
  }
  x
}

#' @rdname edg_delete_attr
#' 
#' @examples 
#' tidy_g %>% 
#'   edg_delete_attr("positive_relation")
#'
#' @export
edg_delete_attr.tbl_graph <- function(x, ...) {
  if (!requireNamespace("tidygraph")) {
    terminate('{tidygraph} is required for this functionality.
               Get it with `install.packages("tidygraph")`')
  }
  tidygraph::as_tbl_graph(edg_delete_attr.igraph(as_igraph.tbl_graph(x), ...))
}


#' Filter edges by their attributes.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param ... `logical` conditions used to subset rows when said conditions evaluate to 
#' `TRUE`.
#' @param edg_attr `character` specifying edge attribute to target. `edg_filter_se()` 
#' only.
#' @param .f `function` (default: `NULL`) specifying the predicate function to apply to
#' `edg_attr`. `edg_filter_se()` only.
#' @param edg_val (default: `NULL`) Value to use as second argument to `.f` (if 
#' applicable).
#' 
#' @return `x`, but with edges filtered by `...` for `edg_filter()` or `edge_attr`, `.f`, 
#' and/or `edg_val` for `edg_filter_se()`.
#' 
#' @details 
#' `edg_filter()` uses non-standard evaluation to make syntax as convenient as possible,
#' while `edg_filter_se()` uses standard evaluation if preferred (or if `edg_filter()` 
#' causes trouble).
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network() 
#' 
#' @export
edg_filter <- function(x, ...) {
  UseMethod("edg_filter")
}


#' @rdname edg_filter
#' 
#' @export
edg_filter_se <- function(x, edg_attr, .f, edg_val = NULL) {
  UseMethod("edg_filter_se")
}

#' @rdname edg_filter
#' 
#' @examples 
#' sampson_monastery %>% 
#'   edg_filter(relation == "liking", time == 1)
#' 
#' @export
edg_filter.bridge_net <- function(x, ...) {
  conditions <- eval(substitute(alist(...)))
  for (i in seq_along(conditions)) {
    call <- tryCatch(eval(conditions[[i]], x[["edges"]]),
                     error = function(e) {
                       validate_edg_attr(x, txt_extract(e[["message"]], "'.*?'"))
                     })
    x[["edges"]] <- x[["edges"]][call, , drop = FALSE]
  }
  x[["metadata"]] <- get_metadata(x)
  x
}


#' @rdname edg_filter
#' 
#' @examples 
#' sampson_monastery %>% 
#'   edg_filter_se("relation", `==`, "liking") %>% 
#'   edg_filter_se("time", `==`, 1)
#' 
#' @export
edg_filter_se.bridge_net <- function(x, edg_attr, .f = NULL, edg_val = NULL) {
  validate_edg_attr(x, edg_attr)
  if (!is.null(.f)) {
    .f <- match.fun(.f)
  }
  if (is.null(edg_val)) {
    if (is.null(.f)) {
      x[["edges"]] <- x[["edges"]][x[["edges"]][[edg_attr]], , drop = FALSE]
    } else {
      x[["edges"]] <- x[["edges"]][.f(x[["edges"]][[edg_attr]]), , drop = FALSE]
    }
  } else {
    x[["edges"]] <- x[["edges"]][.f(x[["edges"]][[edg_attr]], edg_val), , drop = FALSE]
  }
  x[["metadata"]] <- get_metadata(x)
  x
}


#' @rdname edg_filter
#' 
#' @examples 
#' ig %>% 
#'   edg_filter(positive_relation, weight == 3)
#'   
#' @export
edg_filter.igraph <- function(x, ...) {
  as_igraph(edg_filter(as_bridge_net(x), ...))
}


#' @rdname edg_filter
#' 
#' @examples
#' ig %>% 
#'   edg_filter_se("positive_relation") %>% 
#'   edg_filter_se("weight", `==`, 3)
#' 
#' @export
edg_filter_se.igraph <- function(x, edg_attr, .f = NULL, edg_val = NULL) {
  as_igraph(edg_filter_se(as_bridge_net(x), edg_attr, .f, edg_val))
}


#' @rdname edg_filter
#' 
#' @examples 
#' nw %>% 
#'   edg_filter(relation %in% c("blame", "disesteem"))
#'   
#' @export
edg_filter.network <- function(x, ...) {
  as_network(edg_filter(as_bridge_net(x), ...))
}


#' @rdname edg_filter
#' 
#' @examples
#' nw %>% 
#'   edg_filter_se("relation", `%in%`, c("blame", "disesteem"))
#' 
#' @export
edg_filter_se.network <- function(x, edg_attr, .f = NULL, edg_val = NULL) {
  as_network(edg_filter_se(as_bridge_net(x), edg_attr, .f, edg_val))
}


