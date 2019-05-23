#* ====

#' Extract all vertex attributes as a data frame.
#' 
#' @template graph-param
#' 
#' @template bknapp-author
#' 
#' @examples
#' library(snatools)
#' 
#' crisis_in_cloister %>% 
#'   vrt_as_df()

#' florence %>% 
#'   vrt_as_df()
#' 
#' @export
vrt_as_df <- function(x) {
  UseMethod("vrt_as_df")
}

#' @rdname vrt_as_df
#' 
#' @importFrom igraph as_data_frame is_bipartite vcount vertex_attr<- vertex_attr_names
#' @importFrom tibble as_tibble
#' @export
vrt_as_df.igraph <- function(x) {
  if (net_count_vertices.igraph(x) < 1L) {
    return(tibble())
  }
  if (!"name" %in% vertex_attr_names(x)) {
    vertex_attr(x, "name") <- seq_len(vcount(x))
  }
  out <- as_data_frame(x, what = "vertices")
  rownames(out) <- NULL
  out[[".vrt_name"]] <- out[["name"]]
  out[["name"]] <- NULL
  bipartite <- is_bipartite(x)
  if (bipartite) {
    if (!".actor" %in% colnames(out)) {
      out[[".actor"]] <- out[["type"]]
    }
    out[["type"]] <- NULL
  }
  out <- standardize_vrt_cols(out, bipartite = bipartite)
  
  as_tibble(out)
}

#' @rdname vrt_as_df
#' 
#' @importFrom tibble as_tibble tibble
#' @export
vrt_as_df.network <- function(x) {
  if (net_count_vertices.network(x) < 1L) {
    return(tibble())
  }
  attr_names <- unique(unlist(lapply(x[["val"]], names)))
  out <- lapply(attr_names, function(attr) {
    lapply(x[["val"]], `[[`, attr)
    })
  names(out) <- attr_names
  out[["na"]] <- NULL
  nest_test <- unlist(lapply(out, function(x) {
    any(vapply(x, function(y) length(y) != 1L, logical(1L)))
    }))
  to_unlist <- names(nest_test[!nest_test])
  for (i in to_unlist) {
    out[[i]] <- unlist(lapply(out[[i]], `%||%`, NA))
  }
  # TODO consider going with something like this if {purrr} ever imported
  # vrt_df(x) %>%
  #   as_tibble() %>% 
  #   mutate_if(~ all(map_lgl(., ~ length(.x) == 1)),
  #             ~ unlist(.))
  out <- as_tibble(out)
  out[[".vrt_name"]] <- out[["vertex.names"]]
  out[["vertex.names"]] <- NULL
  
  bipartite <- x[["gal"]][["bipartite"]]
  if (is.numeric(bipartite)) {
    out[[".actor"]] <- c(rep(TRUE, times = bipartite), 
                         rep(FALSE, times = x[["gal"]][["n"]] - bipartite))
  }
  out <- standardize_vrt_cols(out, bipartite = is.numeric(bipartite))
  out
}

#* ====

standardize_vrt_cols <- function(x, bipartite) {
  x[[".vrt_id"]] <- seq_len(nrow(x))
  col_names <- colnames(x)
  if (bipartite) {
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

#* ====

#' Extract edge attributes as a data frame.
#' 
#' @template graph-param
#' 
#' @template bknapp-author
#' 
#' @examples
#' library(snatools)
#' 
#' crisis_in_cloister %>% 
#'   edg_as_df()
#'   
#' florence %>% 
#'   edg_as_df()
#' 
#' @export
edg_as_df <- function(x) {
  UseMethod("edg_as_df")
}


#' @rdname edg_as_df
#' 
#' @importFrom igraph as_data_frame is_bipartite edge_attr
#' @importFrom tibble as_tibble
#' @export
edg_as_df.igraph <- function(x) {
  if (net_count_edges.igraph(x) < 1L) {
    return(tibble())
  }
  el <- get_el.igraph(x)
  colnames(el) <- c(".ego", ".alter")
  out <- as_data_frame(x, what = "edges")
  out <- cbind.data.frame(el, out, stringsAsFactors = FALSE)
  out[["from"]] <- NULL
  out[["to"]] <- NULL 
  out <- standardize_edg_cols(out)
  
  as_tibble(out)
}

#' @rdname edg_as_df
#' 
#' @importFrom tibble as_tibble tibble
#' @export
edg_as_df.network <- function(x) {
  if (net_count_edges.network(x) < 1L) {
    return(tibble())
  }
  el <- get_el.network(x)
  colnames(el) <- c(".ego", ".alter")
  peeled_attrs <- lapply(x[["mel"]], `[[`, "atl")
  attr_names <- unique(unlist(lapply(peeled_attrs, names)))
  ### old
  # peeled_attrs <- lapply(x[["mel"]], `[[`, "atl")
  # attr_names <- unique(unlist(lapply(peeled_attrs, names)))
  # pre_df <- lapply(attr_names, function(attr) {
  #   res <- unlist(lapply(lapply(peeled_attrs, `[[`, attr), `%||%`, NA))
  #   res[!is.na(res)]
  #   })
  # names(pre_df) <- attr_names
  # df <- as.data.frame(pre_df, stringsAsFactors = FALSE)
  # 
  # out <- cbind.data.frame(el, df, stringsAsFactors = FALSE)
  # out[["na"]] <- NULL
  # out <- standardize_edg_cols(out)
  ###
  ### new
  out <- lapply(attr_names, function(attr) {
    res <- lapply(lapply(peeled_attrs, `[[`, attr), `%||%`, NA)
    res[!is.na(res)]
    })
  names(out) <- attr_names
  out[["na"]] <- NULL
  if (is_empty(out)) {
    return(standardize_edg_cols(tibble::as_tibble(el)))
  }
  nest_test <- unlist(lapply(out, function(x) {
    any(vapply(x, function(y) length(y) != 1L, logical(1L)))
    }))
  to_unlist <- names(nest_test[!nest_test])
  for (i in to_unlist) {
    out[[i]] <- unlist(lapply(out[[i]], `%||%`, NA))
  }
  ###
  out <- as_tibble(out)
  out <- cbind.data.frame(el, out, stringsAsFactors = FALSE)
  out <- standardize_edg_cols(out)
  as_tibble(out)
}

#* ====

standardize_edg_cols <- function(x) {
  x[[".edg_id"]] <- seq_len(nrow(x))
  col_names <- colnames(x)
  optional_cols <- col_names[!col_names %in% c(".edg_id", ".ego", ".alter")]
  if (!is_empty(optional_cols)) {
    x <- x[, c(".edg_id", ".ego", ".alter", optional_cols)]
  } else {
    x <- x[, c(".edg_id", ".ego", ".alter")]
  }
  `rownames<-`(x, NULL)
}
