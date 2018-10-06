#* ====

#' Extract vertex attributes as a data frame.
#' 
#' @export
vrt_as_df <- function(x, vertex_df = TRUE) {
  UseMethod("vrt_as_df")
}

#' @rdname vrt_as_df
#' 
#' @importFrom igraph as_data_frame is_bipartite vcount vertex_attr<- vertex_attr_names
#' @importFrom tibble as_tibble
#' @export
vrt_as_df.igraph <- function(x) {
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
#' @importFrom tibble as_tibble
#' @export
vrt_as_df.network <- function(x, vertex_df = TRUE) {
  attr_names <- unique(unlist(lapply(x[["val"]], names)))
  out <- lapply(attr_names, function(attr) {
    unlist(lapply(lapply(x[["val"]], `[[`, attr), `%||%`, NA))
    })
  names(out) <- attr_names
  out[["na"]] <- NULL
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out[[".vrt_name"]] <- out[["vertex.names"]]
  out[["vertex.names"]] <- NULL
  
  bipartite <- x[["gal"]][["bipartite"]]
  if (is.numeric(bipartite)) {
    out[[".actor"]] <- c(rep(TRUE, times = bipartite), 
                         rep(FALSE, times = x[["gal"]][["n"]] - bipartite))
  }
  out <- standardize_vrt_cols(out, bipartite = is.numeric(bipartite))
  
  as_tibble(out)
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
#' @importFrom tibble as_tibble
#' @export
edg_as_df.network <- function(x) {
  el <- get_el.network(x)
  colnames(el) <- c(".ego", ".alter")
  
  peeled_attrs <- lapply(x[["mel"]], `[[`, "atl")
  attr_names <- unique(unlist(lapply(peeled_attrs, names)))
  pre_df <- lapply(attr_names, function(attr) {
    res <- unlist(lapply(lapply(peeled_attrs, `[[`, attr), `%||%`, NA))
    res[!is.na(res)]
    })
  names(pre_df) <- attr_names
  df <- as.data.frame(pre_df, stringsAsFactors = FALSE)
  
  out <- cbind.data.frame(el, df, stringsAsFactors = FALSE)
  out[["na"]] <- NULL
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


