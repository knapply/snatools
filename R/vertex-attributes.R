#' Extract a graph objects's vertex attribute names.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return `character` `vector` listing the names of `x`'s vertex attributes.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [vrt_get_attr()], [vrt_to_df()], [igraph::vertex_attr_names()],
#' [network::list.vertex.attributes()]
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
vrt_attr_names <- function(x) {
  UseMethod("vrt_attr_names")
}

#' @rdname vrt_attr_names
#' 
#' @examples 
#' vrt_attr_names(sampson_monastery)
#' 
#' @export
vrt_attr_names.bridge_net <- function(x) {
  names(x[["vertices"]])
}

#' @rdname vrt_attr_names
#' 
#' @seealso [igraph::vertex_attr_names()]
#' 
#' @examples 
#' vrt_attr_names(ig)
#' 
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
#' 
#' @seealso [network::list.vertex.attributes()]
#' 
#' @examples 
#' vrt_attr_names(nw)
#' 
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
#' 
#' @examples 
#' vrt_attr_names(tidy_g)
#' 
#' @export
vrt_attr_names.tbl_graph <- function(x) {
  vrt_attr_names(as_igraph(x))
}

#' @rdname vrt_attr_names
#' 
#' @examples 
#' vrt_attr_names(tidy_g)
#' 
#' @export
vrt_attr_names.adj_matrix <- function(x) {
  rownames(x)
}


#' Extract a specific vertex attribute.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param vrt_attr `character` specifying the target vertex attribute.
#' 
#' @return `vector` of vertex attribute values. May be of any type supported by `x`'s 
#' object class.
#' 
#' @details 
#' * `vrt_get_attr()` allows for the extraction of any `vrt_attr` in `x`.
#'   + If `vrt_attr` is not found, `vrt_get_attr()` considers it to be invalid and throws 
#'     an error.
#'     + The error message will include details on which vertex attributes are valid.
#' * `vrt_get_names()` is a convenience function that extracts the vertex attribute
#'    corresponding to each class' vertex name attribute convention.
#'     + Since `igraph`, `network`, and `tbl_graph` objects do not require that vertices 
#'       have names, `vrt_get_names()` returns an `integer` `vector` corresponding to 
#'       vertex indices if the appropriate attribute is not found.
#'     + For reference, each object class uses the following conventions for vertex names:
#'       + `igraph`: `name`
#'       + `network`: `vertex.names`
#'       + `bridge_net`: `.name`
#'       + `tbl_graph`: `name`
#'    
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [vrt_attr_names()], [vrt_to_df()], [igraph::vertex_attr()],
#' [network::get.vertex.attribute()]
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
vrt_get_attr <- function(x, vrt_attr) {
  UseMethod("vrt_get_attr")
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_attr(sampson_monastery, "faction")
#' 
#' @export
vrt_get_attr.bridge_net <- function(x, vrt_attr) {
  validate_vrt_attr(x, vrt_attr)
  x[["vertices"]][[vrt_attr]]
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_attr(ig, "faction")
#' 
#' @importFrom igraph vertex_attr
#' @export
vrt_get_attr.igraph <- function(x, vrt_attr) {
  validate_vrt_attr(x, vrt_attr)
  vertex_attr(x, vrt_attr)
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_attr(nw, "faction")
#' 
#' @export
vrt_get_attr.network <- function(x, vrt_attr) {
  validate_vrt_attr(x, vrt_attr)
  unlist(lapply(x[["val"]], function(x) {
    x[[vrt_attr]] %||% NA
    }))
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_attr(tidy_g, "faction")
#' 
#' @export
vrt_get_attr.tbl_graph <- function(x, vrt_attr) {
  validate_vrt_attr(x, vrt_attr)
  vrt_get_attr.igraph(as_igraph.tbl_graph(x), vrt_attr)
}

#' @rdname vrt_get_attr
#' 
#' @export
vrt_get_names <- function(x) {
  UseMethod("vrt_get_names")
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_names(sampson_monastery)
#' 
#' @export
vrt_get_names.bridge_net <- function(x) {
  x[["vertices"]][[".name"]]
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_names(ig)
#' 
#' @importFrom igraph vcount vertex_attr
#' @export
vrt_get_names.igraph <- function(x) {
  out <- vertex_attr(x, "name")
  if (is.null(out)) {
    out <- seq_len(vcount(x))
  }
  out
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_names(nw)
#' 
#' @export
vrt_get_names.network <- function(x) {
  unlist(lapply(x[["val"]], `[[`, "vertex.names"))
}

#' @rdname vrt_get_attr
#' 
#' @examples 
#' vrt_get_names(tidy_g)
#' 
#' @export
vrt_get_names.tbl_graph <- function(x) {
  vrt_get_names.igraph(as_igraph.tbl_graph(x))
}

#' Extract all vertex attributes as a data frame.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param leave_raw `logical` (default: `FALSE`) indicating whether to return
#' a raw `data.frame` instead of a `vertex_data_frame` that standardizes results across 
#' graph classes.
#' 
#' @return `data.frame` or `vertex_data_frame` with all vertex attributes contained in `x`.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [vrt_get_attr()], [vrt_attr_names()], [igraph::as_data_frame()]
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
vrt_to_df <- function(x, leave_raw = FALSE) {
  UseMethod("vrt_to_df")
}

#' @rdname vrt_to_df
#' 
#' @examples 
#' vrt_to_df(sampson_monastery)
#' 
#' @export
vrt_to_df.bridge_net <- function(x) {
  x[["vertices"]]
}

#' @rdname vrt_to_df
#' 
#' @examples 
#' vrt_to_df(ig)
#' 
#' @importFrom igraph as_data_frame set_vertex_attr V vcount vertex_attr
#' @export
vrt_to_df.igraph <- function(x, leave_raw = FALSE) {
  if (net_count_vertices(x) == 0L) {
    return(NULL)
  }
  if (!"name" %in% vrt_attr_names(x)) {
    x <- set_vertex_attr(x, "name", value = seq_len(vcount(x)))
  }
  if (!leave_raw && net_is_bipartite(x)) {
    x <- prep_bipartite_igraph(x)
  }
  out <- as_data_frame(x, what = "vertices")
  rownames(out) <- NULL
  if (!leave_raw) {
    if (net_is_bipartite(x)) {
      out[[".actor"]] <- out[["type"]]
      out[["type"]] <- NULL
    }
    return(as_vertex_data_frame(out))
  }
  out
}

#' @rdname vrt_to_df
#' 
#' @examples 
#' vrt_to_df(nw)
#' 
#' @export
vrt_to_df.network <- function(x, leave_raw = FALSE) {
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
  if (!leave_raw) {
    if (net_is_bipartite(x)) {
      out[[".actor"]] <- c(rep(TRUE, x[["gal"]][["bipartite"]]),
                           rep(FALSE, x[["gal"]][["n"]] - x[["gal"]][["bipartite"]]))
    }
    return(as_vertex_data_frame(out))
  }
  out
}

#' @rdname vrt_to_df
#' 
#' @examples 
#' vrt_to_df(tidy_g)
#' 
#' @export
vrt_to_df.tbl_graph <- function(x, leave_raw = FALSE) {
  vrt_to_df(as_igraph(x), leave_raw = leave_raw)
}


as_vertex_data_frame <- function(x) {
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
  class(x) <- c("vertex_data_frame", "data.frame")
  x
}

as.data.frame.vertex_data_frame <- function(x) {
  as_df(x)
}

print.vertex_data_frame <- function(x, .nrow = 10L, .verbose = TRUE) {
  out <- head(x, .nrow)
  rownames(out) <- paste0("  ", seq_len(nrow(out)))
  if (.verbose) {
    cat_patch("# vertex_data_frame: %s x %s",
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
    cat("\n")
  }
  cat("\n")
  invisible(x)
}


#' Delete Vertex Attributes
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param ... `character` including the name(s) of one or more vertex attributes to delete 
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
vrt_delete_attr <- function(x, ...) {
  UseMethod("vrt_delete_attr")
}

#' @rdname vrt_delete_attr
#' 
#' @examples 
#' sampson_monastery %>% 
#'   vrt_delete_attr("cloisterville", "status")
#'   
#' @export
vrt_delete_attr.bridge_net <- function(x, ...) {
  attrs <- c(...)
  validate_vrt_attr(x, attrs, max_length = Inf)
  x[["vertices"]][, attrs] <- NULL
  x
}

#' @rdname vrt_delete_attr
#' 
#' @seealso [`igraph::delete_vertex_attr()`]
#' 
#' @examples 
#' ig %>% 
#'   vrt_delete_attr("faction")
#' 
#' @importFrom igraph delete_vertex_attr
#' @export
vrt_delete_attr.igraph <- function(x, ...) {
  attrs <- c(...)
  validate_vrt_attr(x, attrs, max_length = Inf)
  for (i in seq_along(attrs)) {
    x <- delete_vertex_attr(x, attrs[[i]])
  }
  x
}

#' @rdname vrt_delete_attr
#' 
#' @seealso [`network::delete.vertex.attribute()`]
#' 
#' @examples 
#' nw %>% 
#'   vrt_delete_attr("cloisterville", "faction")
#' 
#' @importFrom network delete.vertex.attribute
#' @export
vrt_delete_attr.network <- function(x, ...) {
  attrs <- c(...)
  validate_vrt_attr(x, attrs, max_length = Inf)
  for (i in seq_along(attrs)) {
    network::delete.vertex.attribute(x, attrs[[i]])
  }
  x
}

#' @rdname vrt_delete_attr
#' 
#' @examples 
#' tidy_g %>% 
#'   vrt_delete_attr("faction", "status")
#'
#' @export
vrt_delete_attr.tbl_graph <- function(x, ...) {
  if (!requireNamespace("tidygraph")) {
    terminate('{tidygraph} is required for this functionality.
               Get it with `install.packages("tidygraph")`')
  }
  tidygraph::as_tbl_graph(vrt_delete_attr.igraph(as_igraph.tbl_graph(x), ...))
}

#' Filter vertices by their attributes.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param ... `logical` conditions used to subset rows when said conditions evaluate to 
#' `TRUE`.
#' @param vrt_attr `character` specifying edge attribute to target. `vrt_filter_se()` 
#' only.
#' @param .f `function` (default: `NULL`) specifying the predicate function to apply to
#' `vrt_attr`. `vrt_filter_se()` only.
#' @param vrt_val (default: `NULL`) Value to use as second argument to `.f` (if 
#' applicable).
#' 
#' @return `x`, but with vertices filtered by `...` for `vrt_filter()` or `edge_attr`, `.f`, 
#' and/or `vrt_val` for `vrt_filter_se()`.
#' 
#' @details 
#' `vrt_filter()` uses non-standard evaluation to make syntax as convenient as possible,
#' while `vrt_filter_se()` uses standard evaluation if preferred (or if `vrt_filter()` 
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
vrt_filter <- function(x, ...) {
  UseMethod("vrt_filter")
}


#' @rdname vrt_filter
#' 
#' @export
vrt_filter_se <- function(x, vrt_attr, .f, vrt_val = NULL) {
  UseMethod("vrt_filter_se")
}

#' @rdname vrt_filter
#' 
#' @examples 
#' sampson_monastery %>% 
#'   vrt_filter(faction == "Outcasts", status == "Expelled")
#' 
#' @export
vrt_filter.bridge_net <- function(x, ...) {
  conditions <- eval(substitute(alist(...)))
  x[["vertices"]][["old_index"]] <- seq_len(nrow(x[["vertices"]]))
  for (i in seq_along(conditions)) {
    call <- tryCatch(eval(conditions[[i]], x[["vertices"]]),
                     error = function(e) {
                       validate_vrt_attr(x, txt_extract(e[["message"]], "'.*?'"))
                     })
    x[["vertices"]] <- x[["vertices"]][call, , drop = FALSE]
  }
  x[["edges"]] <- x[["edges"]][x[["edges"]][, ".ego"] %in% 
                                 x[["vertices"]][["old_index"]], ]
  x[["edges"]] <- x[["edges"]][x[["edges"]][, ".alter"] %in% 
                                 x[["vertices"]][["old_index"]], ]
  
  x[["edges"]][, ".ego"] <- match(x[["edges"]][, ".ego"], 
                                  x[["vertices"]][["old_index"]])
  x[["edges"]][, ".alter"] <- match(x[["edges"]][, ".alter"], 
                                    x[["vertices"]][["old_index"]])
  
  x[["vertices"]][["old_index"]] <- NULL
  x[["metadata"]] <- get_metadata(x)
  x
}


#' @rdname vrt_filter
#' 
#' @examples 
#' sampson_monastery %>% 
#'   vrt_filter_se("faction", `==`, "Outcasts") %>% 
#'   vrt_filter_se("status", `==`, "Expelled")
#' 
#' @export
vrt_filter_se.bridge_net <- function(x, vrt_attr, .f = NULL, vrt_val = NULL) {
  validate_vrt_attr(x, vrt_attr)
  x[["vertices"]][["old_index"]] <- seq_len(nrow(x[["vertices"]]))
  if (!is.null(.f)) {
    .f <- match.fun(.f)
  }
  if (is.null(vrt_val)) {
    if (is.null(.f)) {
      x[["vertices"]] <- x[["vertices"]][x[["vertices"]][[vrt_attr]], , drop = FALSE]
    } else {
      x[["vertices"]] <- x[["vertices"]][.f(x[["vertices"]][[vrt_attr]]), , drop = FALSE]
    }
  } else {
    x[["vertices"]] <- x[["vertices"]][.f(x[["vertices"]][[vrt_attr]], vrt_val), , drop = FALSE]
  }
  x[["edges"]] <- x[["edges"]][x[["edges"]][, ".ego"] %in% 
                                 x[["vertices"]][["old_index"]], ]
  x[["edges"]] <- x[["edges"]][x[["edges"]][, ".alter"] %in% 
                                 x[["vertices"]][["old_index"]], ]
  
  x[["edges"]][, ".ego"] <- match(x[["edges"]][, ".ego"], 
                                  x[["vertices"]][["old_index"]])
  x[["edges"]][, ".alter"] <- match(x[["edges"]][, ".alter"], 
                                    x[["vertices"]][["old_index"]])
  
  x[["vertices"]][["old_index"]] <- NULL
  
  x[["metadata"]] <- get_metadata(x)
  x
}

#' @rdname vrt_filter
#' 
#' @examples 
#' ig %>% 
#'   vrt_filter(cloisterville, faction == "Loyal")
#'   
#' @export
vrt_filter.igraph <- function(x, ...) {
  as_igraph(vrt_filter(as_bridge_net(x), ...))
}


#' @rdname vrt_filter
#' 
#' @examples
#' ig %>% 
#'   vrt_filter_se("cloisterville") %>% 
#'   vrt_filter_se("faction", `==`, "Loyal")
#' 
#' @export
vrt_filter_se.igraph <- function(x, vrt_attr, .f = NULL, vrt_val = NULL) {
  as_igraph(vrt_filter_se(as_bridge_net(x), vrt_attr, .f, vrt_val))
}


#' @rdname vrt_filter
#' 
#' @examples 
#' nw %>% 
#'   vrt_filter(status %in% c("Expelled", "Left Voluntarily"))
#'   
#' @export
vrt_filter.network <- function(x, ...) {
  as_network(vrt_filter(as_bridge_net(x), ...))
}


#' @rdname vrt_filter
#' 
#' @examples
#' nw %>% 
#'   vrt_filter_se("status", `%in%`, c("Expelled", "Left Voluntarily"))
#' 
#' @export
vrt_filter_se.network <- function(x, vrt_attr, .f = NULL, vrt_val = NULL) {
  as_network(vrt_filter_se(as_bridge_net(x), vrt_attr, .f, vrt_val))
}

