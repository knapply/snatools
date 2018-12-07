# 
# 
# #* ====
# #' Extract all vertex attributes as a data frame.
# #' 
# #' @template graph-param
# #' 
# #' @template bknapp-author
# #' 
# #' @examples
# #' library(snatools)
# #' 
# #' crisis_in_cloister %>% 
# #'   vrt_as_df()
# 
# #' florence %>% 
# #'   vrt_as_df()
# #' 
# #' @export
# vrt_as_df <- function(x) {
#   UseMethod("vrt_as_df")
# }
# 


# * new col standardizers * ====
standardize_vrt_cols <- function(x, .class, .bipartite) {
  if (.class == "igraph") {
    if (!"name" %in% colnames(x)) {
      x[[".vrt_name"]] <- x[[".vrt_id"]]
    } else {
      x[[".vrt_name"]] <- x[["name"]]
      x[["name"]] <- NULL
    }
  }
  if (.class == "network") {
    if (!"vertex.names" %in% colnames(x)) {
      x[[".vrt_name"]] <- x[[".vrt_id"]]
    } else {
      x[[".vrt_name"]] <- x[["vertex.names"]]
      x[["vertex.names"]] <- NULL
    }
  }
  col_names <- colnames(x)
  if (.bipartite) {
    optional_cols <- col_names[!col_names %in% c(".vrt_id", ".vrt_name", ".actor")]
    if (!is_empty(optional_cols)) {
      x <- x[, c(".vrt_id", ".vrt_name", ".actor", optional_cols)]
    } else {
      x <- x[, c(".vrt_id", ".vrt_name", ".actor")]
    }
    return(x)
  }
  optional_cols <- col_names[!col_names %in% c(".vrt_id", ".vrt_name")]
  if (!is_empty(optional_cols)) {
    x <- x[, c(".vrt_id", ".vrt_name", optional_cols)]
  } else {
    x <- x[, c(".vrt_id", ".vrt_name")]
  }
  `rownames<-`(x, NULL)
}

standardize_edg_cols <- function(x) {
  # x[[".edg_id"]] <- seq_len(nrow(x))
  col_names <- colnames(x)
  optional_cols <- col_names[!col_names %in% c(".edg_id", ".ego", ".alter")]
  if (!is_empty(optional_cols)) {
    x <- x[, c(".edg_id", ".ego", ".alter", optional_cols)]
  } else {
    x <- x[, c(".edg_id", ".ego", ".alter")]
  }
  `rownames<-`(x, NULL)
}

# * new data frames * ====
as.data.frame.network <- function(x, .unit = c("edges", "vertices")) {
  .unit <- match.arg(.unit, c("edges", "vertices"))
  if (.unit == "edges") {
    edge_ids <- which(!vapply(x[["mel"]], is.null, logical(1L))) 
    egos <- lapply(x[["mel"]], `[[`, "outl")[edge_ids]
    alters <- lapply(x[["mel"]], `[[`, "inl")[edge_ids]
    if (x[["gal"]][["hyper"]]) {
      first_cols <- data.frame(.edg_id = edge_ids,
                               .ego = egos,
                               .alter = alters,
                               stringsAsFactors = FALSE)
    } else {
      first_cols <- data.frame(.edg_id = edge_ids,
                               .ego = as.integer(unlist(egos)),
                               .alter = as.integer(unlist(alters)),
                               stringsAsFactors = FALSE)
    }
    attr_names <- edg_attr_names(x)
    if (is.null(attr_names)) {
      return(standardize_edg_cols(first_cols))
    }
    out <- lapply(attr_names, function(e_attr) {
      edg_get_attr(x, e_attr)[edge_ids]
    })
    names(out) <- attr_names
    if (".edg_id" %in% attr_names) {
      first_cols[[".edg_id"]] <- out[[".edg_id"]]
      out[[".edg_id"]] <- NULL
    }
  }
  if (.unit == "vertices") {
    first_cols <- data.frame(.vrt_id = which(!vapply(x[["val"]], is.null, logical(1L))))
    attr_names <- vrt_attr_names(x)
    if (is.null(attr_names)) {
      return(standardize_vrt_cols(first_cols, .class = "network", 
                                  .bipartite = net_is_bipartite(x)))
    }
    out <- lapply(attr_names, function(v_attr) {
      vrt_get_attr(x, v_attr)
    })
    names(out) <- attr_names
    if (".vrt_id" %in% attr_names) {
      first_cols[[".vrt_id"]] <- out[[".vrt_id"]]
      out[[".vrt_id"]] <- NULL
    }
    bipartite <- x[["gal"]][["bipartite"]]
    if (is.numeric(bipartite)) {
      out[[".actor"]] <- c(rep(TRUE, times = bipartite), 
                           rep(FALSE, times = x[["gal"]][["n"]] - bipartite))
    }
  }
  
  list_col_index <- !vapply(out, is_simplifiable, logical(1L))
  out[list_col_index] <- lapply(out[list_col_index], I)
  atomic_col_names <- names(list_col_index[!list_col_index])
  out[atomic_col_names] <- lapply(out[atomic_col_names], unlist)

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out[list_col_index] <- lapply(out[list_col_index], `class<-`, "list")
  out <- cbind.data.frame(first_cols, out, stringsAsFactors = FALSE)

  switch(.unit,
         vertices = standardize_vrt_cols(out, .class = "network", 
                                         .bipartite = net_is_bipartite(x)),
         edges = standardize_edg_cols(out))
}


as.data.frame.igraph <- function(x, .unit = c("edges", "vertices")) {
  .unit <- match.arg(.unit, c("edges", "vertices"))
  if (.unit == "edges") {
    el <- rep_as_edgelist(x, use_names = FALSE)
    first_cols <- data.frame(.edg_id = seq_len(nrow(el)),
                             .ego = el[, 1L],
                             .alter = el[, 2L])
    attr_names <- edg_attr_names(x)
    if (is.null(attr_names)) {
      return(standardize_edg_cols(first_cols))
    }
    out <- lapply(attr_names, function(e_attr) {
      edg_get_attr(x, e_attr)
    })
    names(out) <- attr_names
    if (".edg_id" %in% attr_names) {
      first_cols[[".edg_id"]] <- out[[".edg_id"]]
      out[[".edg_id"]] <- NULL
    }
  }
  if (.unit == "vertices") {
    first_cols <- data.frame(.vrt_id = seq_len(net_count_vertices(x)))
    attr_names <- vrt_attr_names(x)
    if (is.null(attr_names)) {
      return(standardize_vrt_cols(first_cols, .class = "igraph",
                                  .bipartite = net_is_bipartite(x)))
    }
    out <- lapply(attr_names, function(v_attr) {
      vrt_get_attr(x, v_attr)
    })
    names(out) <- attr_names
    if (".vrt_id" %in% attr_names) {
      first_cols[[".vrt_id"]] <- out[[".vrt_id"]]
      out[[".vrt_id"]] <- NULL
    }
    
    if (net_is_bipartite(x)) {
      if (!".actor" %in% colnames(out)) {
        out[[".actor"]] <- out[["type"]]
      }
      out[["type"]] <- NULL
    }
  }
  list_col_index <- !vapply(out, is_simplifiable, logical(1L))
  out[list_col_index] <- lapply(out[list_col_index], I)
  atomic_col_names <- names(list_col_index[!list_col_index])
  out[atomic_col_names] <- lapply(out[atomic_col_names], unlist)

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out[list_col_index] <- lapply(out[list_col_index], `class<-`, "list")
  out <- cbind.data.frame(first_cols, out, stringsAsFactors = FALSE)

  switch(.unit,
         vertices = standardize_vrt_cols(out, .class = "igraph", 
                                         .bipartite = net_is_bipartite(x)),
         edges = standardize_edg_cols(out))
}

as.data.frame.tbl_graph <- function(x, .unit = c("edges", "vertices")) {
  as.data.frame(as_igraph(x), .unit = .unit)
}

# * new tibbles and data.tables * ====
as_tibble.network <- function(x, .unit = c("edges", "vertices")) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    glue_stop('`tibble` package is required for this operation.
              Get it with `install.packages("tibble")`.')
  }
  tibble::as_tibble(as.data.frame.network(x, .unit = .unit))
}

as.data.table.network <- function(x, .unit = c("edges", "vertices")) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    glue_stop('`data.table` package is required for this operation.
              Get it with `install.packages("data.table")`.')
  }
  data.table::as.data.table(as.data.frame.network(x, .unit = .unit))
}

as_tibble.igraph <- function(x, .unit = c("edges", "vertices")) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    glue_stop('`tibble` package is required for this operation.
              Get it with `install.packages("tibble")`.')
  }
  tibble::as_tibble(as.data.frame.igraph(x, .unit = .unit))
}

as.data.table.igraph <- function(x, .unit = c("edges", "vertices")) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    glue_stop('`data.table` package is required for this operation.
              Get it with `install.packages("data.table")`.')
  }
  data.table::as.data.table(as.data.frame.igraph(x, .unit = .unit))
}


vrt_as_df <- function(x) {
  as.data.frame(x, .unit = "vertices")
}

edg_as_df <- function(x) {
  as.data.frame(x, .unit = "edges")
}








