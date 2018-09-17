rep_as_edgelist <- function(x, use_names = TRUE, vrt_attr = NULL) {
  UseMethod("rep_as_edgelist")
}

#' @importFrom igraph as_edgelist
rep_as_edgelist <- function(x, use_names = TRUE, vrt_attr = NULL, leave_raw = FALSE) {
  validate_graph(x)
  if (use_names && is.null(vrt_attr)) {
    vrt_attr <- get_vrt_names_attr(x)
  }
  if (!is.null(vrt_attr)) {
    validate_vrt_attr(x, vrt_attr)
  }
  out <- get_el(x, vrt_attr)
  if (leave_raw) {
    attr(out, "el_type") <- NULL
    attr(out, "vrt_attr_name") <- NULL
    return(out)
  }
  out <- set_metadata_attr(out, graph = x)
  set_edgelist_class(out)
}



get_el <- function(x, vrt_attr = NULL) {
  UseMethod("get_el")
}

#' @importFrom igraph as_edgelist vertex_attr
get_el.igraph <- function(x, vrt_attr = NULL) {
  el <- as_edgelist(x, names = FALSE)
  if (is.null(vrt_attr)) {
    el_type <- "vrt_indices"
    el <- set_whole_number_storage(el)
  } else if (vrt_attr == "name") {
    el_type <- "vrt_names"
  } else {
    el_type <- "vrt_attrs"
  }
  if (!is.null(vrt_attr)) {
    el <- matrix(vertex_attr(x, vrt_attr)[el], ncol = 2L)
  }
  colnames(el) <- c(".ego", ".alter")
  rownames(el) <- NULL
  attr(el, "el_type") <- el_type
  if (el_type == "vrt_attrs") {
    attr(el, "vrt_attr_name") <- vrt_attr
  }
  el
}

get_el.network <- function(x, vrt_attr = NULL) {
  outl <- unlist(lapply(x[["mel"]], `[[`, "outl"))
  inl <- unlist(lapply(x[["mel"]], `[[`, "inl"))
  el <- vector(typeof(outl), length(outl) * 2L)
  if (net_is_directed(x) && !net_is_bipartite(x)) {
    el <- cbind(outl, inl)
  } 
  if (net_is_directed(x) && net_is_bipartite(x)) {
    el <- cbind(outl, inl)
  }
  if (!net_is_directed(x)) {
    if (!net_is_bipartite(x)) {
      el <- cbind(inl, outl)
      el <- t(apply(el, 1, FUN = sort))
    } else {
      el <- cbind(inl, outl)
      el <- t(apply(el, 1, FUN = sort))
    }
  }
  if (is.null(vrt_attr)) {
    el_type <- "vrt_indices"
    el <- set_whole_number_storage(el)
  } else if (vrt_attr == "vertex.names") {
    el_type <- "vrt_names"
    el <- matrix(vrt_get_attr(x, "vertex.names")[el], ncol = 2L)
  } else {
    el_type <- "vrt_attrs"
    el <- matrix(vrt_get_attr(x, vrt_attr)[el], ncol = 2L)
  }
  colnames(el) <- c(".ego", ".alter")
  rownames(el) <- NULL
  attr(el, "el_type") <- el_type
  if (el_type == "vrt_attrs") {
    attr(el, "vrt_attr_name") <- vrt_attr
  }
  el
}


get_el.tbl_graph <- function(x, vrt_attr = NULL) {
  get_el(as_igraph(x), vrt_attr = vrt_attr)
}

set_edgelist_class <- function(x) {
  class(x) <- c("edgelist", "matrix")
  x
}

#' @export
is_edgelist <- function(x) {
  inherits(x, "edgelist")
}

#' @export
print.edgelist <- function(x, .nrow = 10L) {
  out <- head(x, .nrow)
  rownames(out) <- paste0(seq_len(nrow(out)))
  
  el_type <- switch(attr(x, "el_type"),
                    vrt_names = "vertex names",
                    vrt_indices = "vertex indices",
                    patch("a vertex attribute ('%s').", attr(x, "vrt_attr_name")))
  
  direction <- ifelse(attr(x, "is_directed"), " directed", "n undirected")
  bipart <- ifelse(attr(x, "is_bipartite"), ", bipartite", "")
  
  cat_patch("# A%s%s `edgelist` with %s edges.",
            direction,
            bipart,
            format(nrow(x), big.mark = ","))
  cat("\n")
  cat_patch("# Values correspond to %s.", el_type)
  cat("\n")
  print.table(out)
  if (nrow(out) < nrow(x)) {
    cat_patch("# ... with %s additional rows.", nrow(x) - nrow(out))
  }
}

#' @export
as.matrix.edgelist <- function(x) {
  attr_names <- names(attributes(x))
  attr_names <- attr_names[!attr_names %in% c("dim", "dimnames")]
  for (i in seq_along(attr_names)) {
    attr(x, attr_names[[i]]) <- NULL
  }
  class(x) <- "matrix"
  x
}

#' @export
as.data.frame.edgelist <- function(x, stringsAsFactors = default.stringsAsFactors(), ...) {
  x <- as.data.frame(as.matrix(x), stringsAsFactors = stringsAsFactors, ...)
  x
}




#' 
#' 
#' 
#' rep_as_edgelist.sna_net <- function(x, vrt_attr = ".name") {
#'   if (!is.character(vrt_attr) || length(vrt_attr) > 1L || !is.atomic(vrt_attr)) {
#'     stop("`vrt_attr` must be a character vector of length 1.", call. = FALSE)
#'   }
#'   if (is.null(vrt_attr)) {
#'     return(as_sna_edgelist(x[["edges"]][, c(".ego", ".alter")],
#'                            el_type = "vrt_indices"))
#'   }
#'   if (!vrt_attr %in% vrt_attr_names(x)) {
#'     vrt_attrs <- colnames(x[["vertices"]])
#'     stop(paste0('"', vrt_attr, '"'),
#'          " is not a vertex attribute in `x`.", "\n",
#'          "`x`'s vertex attributes are:\n", 
#'          paste0('\t- "', vrt_attrs, '"', collapse = "\n"),
#'          call. = FALSE)
#'   }
#'   if (is_null(vrt_attr)) {
#'     el_type <- "vrt_indices"
#'   } else {
#'     el_type <- switch(vrt_attr,
#'                       vrt_names = "name",
#'                       vrt_names = ".name",
#'                       "vrt_attr")
#'   }
#'   
#'   ego <- x[["edges"]][, ".ego"]
#'   ego <- vrt_get_attr(x, vrt_attr)[ego]
#'   alter <- x[["edges"]][, ".alter"]
#'   alter <- vrt_get_attr(x, vrt_attr)[alter]
#'   
#'   out <- cbind(ego, alter)
#'   
#'   as_sna_edgelist(out, el_type)
#' }
#' 
#' 
#' #' @importFrom igraph as_edgelist vertex_attr_names
#' #' @export
#' rep_as_edgelist.igraph <- function(x, vrt_attr = ".name") {
#'   if (is.null(vrt_attr)) {
#'     out <- as_edgelist(x, names = FALSE)
#'     return(as_sna_edgelist(out))
#'   }
#'   if (vrt_attr %in% c(".name", "name")) {
#'     out <- as_edgelist(x, names = TRUE)
#'     return(as_sna_edgelist(out))
#'   }
#'   if (!vrt_attr %in% vrt_attr_names(x)) {
#'     vattr_names <- vrt_attr_names(x)
#'     stop("\n\n",
#'          paste0('"', vrt_attr, '"'),
#'          " is not a vertex attribute in `x`.", "\n\n",
#'          "`x`'s vertex attributes are:\n", 
#'          paste0('\t- "', vattr_names, '"', collapse = "\n"))
#'   }
#'   attr <- vrt_get_attr(vrt_attr)
#'   if (!vrt_attr %in% vrt_attr_names(x)) {
#'     
#'   }
#'   return(vrt_attr)
#'   as_sna_edgelist(out)
#' }
#' 
#' # test_ig %>% rep_as_edgelist("face")
#' 
#' 
#' 
#' 
#' 
#' rep_as_edgelist.network <- function(x, vrt_attr) {
#'   outl <- unlist(lapply(x[["mel"]], `[[`, "outl"))
#'   inl <- unlist(lapply(x[["mel"]], `[[`, "inl"))
#'   if (typeof(outl) != typeof(inl)) {
#'     outl <- as.integer(outl)
#'     inl <- as.integer(inl)
#'   }
#'   out <- vector(typeof(outl), length(outl) * 2L)
#'   if (net_is_directed(x)&& !net_is_bipartite(x)) {
#'     out <- cbind(outl, inl)
#'   } 
#'   if (net_is_directed(x) && net_is_bipartite(x)) {
#'     out <- cbind(outl, inl)
#'   }
#'   if (!net_is_directed(x)) {
#'     out <- cbind(outl, inl)
#'     out <- t(apply(el, 1, FUN = sort))
#'   }
#'   
#'   
#'   
#'   as_sna_edgelist(out)
#' }
#' 
#' rep_as_edgelist.tbl_graph <- function(x, vrt_attr) {
#'   rep_as_edgelist(as_igraph(x), vrt_attr)
#' }
#' 


