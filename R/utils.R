`%||%` <- function(lhs, rhs) {
  if (identical(lhs, NULL)) return(rhs)
  lhs
}

`%{}%` <- function(lhs, rhs) {
  if (length(lhs) == 0L) return(rhs)
  lhs
}

#' @export
set_whole_number_storage <- function(x, big_tol = .Machine$integer.max*0.8, verbose = FALSE) {
  stopifnot(is.numeric(x))
  if (is.integer(x)) {
    return(x)
  }
  if (any(x >= big_tol)) {
    if (verbose) {
      message(patch("values >= %s billion present, leaving `x` as is.",
                    .Machine$integer.max * 0.8))
    }
    return(x)
  }
  `storage.mode<-`(x, "integer")
}


is_null <- function(x) {
  identical(x, NULL)
}

is_empty <- function(x) {
  length(x) == 0L
}

is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1L
}


is_true <- function(x) {
  identical(x, TRUE) & isTRUE(x)
}

is_named <- function(x) {
  le_names <- names(x)
  if (is_null(le_names)) {
    return(FALSE)
  }
  if (is_scalar(x)) {
    return(le_names == "" || is.na(x))
  }
  all(le_names == "")|| all(is.na(x))
}

is_valid_graph <- function(x) {
  inherits(x, c("bridge_net", "igraph", "network", "tbl_graph"))
}

validate_graph <- function(x) {
  if (!is_valid_graph(x)) {
    terminate("`x` must be an `bridge_net`, `igraph`, `network`, or `tbl_graph`.")
  }
}

# dots <- function(...) {
#   eval(substitute(alist(...)))
# }


terminate <- function(..., call. = FALSE) {
  messages <- c(...)
  stop(vapply(messages, txt_wrap, character(1)), call. = call.)
}

#' @importFrom igraph permute set_vertex_attr vcount vertex_attr_names
prep_bipartite_igraph <- function(x) {
  if (!"name" %in% vertex_attr_names(x)) {
    x <- set_vertex_attr(x, "name", value = seq_len(vcount(x)))
  }
  type_order <- vertex_attr(x, "type")
  names(type_order) <- seq_along(type_order)
  new_order <- as.integer(names(sort(type_order, decreasing = TRUE)))
  x <- permute(x, match(seq_along(new_order), new_order))
  x
}

format_table <- function(x, edges_or_verts, specify_edge_dyad_types = TRUE) {
  trunc_types <- c("character" = "<chr>", "double" = "<dbl>", "factor" = "<fct>",
                   "integer" = "<int>", "logical" = "<lgl>")
  if (edges_or_verts == "edges" && is.matrix(x) && is.integer(x)) {
    types <- matrix(rep("<.index>", 2L), nrow = 1L)
    colnames(types) <- c(".ego", ".alter")
    out <- rbind(types, x)
    rownames(out) <- c(" ", rep(" |", nrow(out) - 1L))
    return(out)
  }
  if (edges_or_verts == "edges" && is.matrix(x) && is.character(x)) {
    types <- matrix(rep("<.name>", 2L), nrow = 1L)
    colnames(types) <- c(".ego", ".alter")
    out <- rbind(types, x)
    rownames(out) <- c(" ", rep(" |", nrow(out) - 1L))
    return(out)
  }
  
  .nrow <- nrow(x)
  types <- vapply(as.list(x[1, ]), typeof, character(1L), USE.NAMES = FALSE)
  types <- trunc_types[types]
  names(types) <- NULL
  
  types <- matrix(types, nrow = 1L)
  colnames(types) <- colnames(x)
  x <- as.matrix(x)
  out <- rbind(types, x)
  if (edges_or_verts == "edges" && specify_edge_dyad_types) {
    out[1, 1:2] <- "<.name>"
    rownames(out) <- c(rep("  ", 1L), rep(" |", nrow(out) - 1L))
  }
  if (edges_or_verts == "verts") {
    out[1, 1] <- "<.name>"
    rownames(out) <- c("  ", rep(" |", nrow(out) - 1L))
  }
  out
}

strip_added_attrs <- function(x) {
  UseMethod("strip_added_attrs")
}

strip_added_attrs.data.frame <- function(x) {
  stopifnot(is.data.frame(x))
  to_keep <- c("names", "row.names", "class")
  current_attrs <- names(attributes(x))
  to_strip <- current_attrs[!current_attrs %in% to_keep]
  if (!is_empty(to_strip)) {
    for (i in to_strip) {
      attr(x, i) <- NULL
    }
  }
  class(x) <- "data.frame"
  x
}

strip_added_attrs.matrix <- function(x) {
  stopifnot(is.matrix(x))
  to_keep <- c("dim", "dimnames")
  current_attrs <- names(attributes(x))
  to_strip <- current_attrs[!current_attrs %in% to_keep]
  if (!is_empty(to_strip)) {
    for (i in to_strip) {
      attr(x, i) <- NULL
    }
  }
  class(x) <- "matrix"
  x
}

as_df <- function(x) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  stopifnot(rownames(x) == seq_len(nrow(x)))
  strip_added_attrs(x)
}

as_matrix <- function(x) {
  stopifnot(is.matrix(x))
  # stopifnot(rownames(x) == seq_len(nrow(x)))
  strip_added_attrs(x)
}



