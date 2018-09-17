#' #' @importFrom rlang is_named
#' sort_list_by_name <- function(x) {
#'   if(is_named(x)) {
#'     return(x[order(names(x))])
#'   }
#'   x
#' }
#' 
#' clear_metadata <- function(x) {
#'   metadata <- c("n", "directed", "hyper", "loops", "multiple", "bipartite", "mnext")
#'   out <- x[!names(x) %in% metadata]
#'   sort_list_by_name(out)
#' }
#' 
#' dissect_graph <- function(x) {
#'   net_attrs = net_get_attrs(x)
#'   if (length(unlist(net_attrs)) == 0L | all(is.na(unlist(net_attrs)))) {
#'     net_attrs <- NULL
#'   }
#'   vrt_attrs = vrt_get_attrs(x)
#'   if ("is_actor" %in% vrt_get_attr_names(x)) {
#'     vrt_attrs$type <- vrt_attrs$is_actor
#'     vrt_attrs$is_actor <- NULL
#'   }
#'   edg_attrs = edg_get_attrs(x)
#'   el <- rep_edgelist(x)
#'   
#'   if(class(x) == "network") {
#'     vrt_attrs$name <- vrt_attrs$vertex.names
#'     vrt_attrs$vertex.names <- NULL
#'     net_attrs <- clear_metadata(net_attrs)
#'   }
#'   out <- list(net_attrs = sort_list_by_name(net_attrs),
#'               vrt_attrs = sort_list_by_name(vrt_attrs),
#'               edg_attrs = sort_list_by_name(edg_attrs),
#'               el = el)
#'   out
#' }
#' 
#'   
#' 
#' clean_network_metadata <- function(x) {
#'   if(class(x) != "network") {
#'     stop("`fill_network_metadata()` is only applicable to `network` objects.")
#'   }
#'   if(is.null(x$gal$bipartite)) {
#'     network::set.network.attribute(x, "bipartite", network::is.bipartite(x))
#'   }
#'   if(is.null(x$gal$loops)) {
#'     network::set.network.attribute(x, "loops", network::has.loops(x))
#'   }
#'   if(is.null(x$gal$hyper)) {
#'     network::set.network.attribute(x, "hyper", network::is.hyper(x))
#'   }
#'   if(is.null(x$gal$multiple)) {
#'     if(is.null(network::is.multiplex(x))) {
#'       network::set.network.attribute(x, "multiple", FALSE)
#'     } else {
#'     network::set.network.attribute(x, "multiple", network::is.multiplex(x))
#'     }
#'   }
#'   net_attrs <- net_get_attrs(x, drop_metadata = FALSE)
#'   net_attrs <- net_attrs[order(names(net_attrs))]
#'   for(i in names(net_attrs)) {
#'     network::delete.network.attribute(x, i)
#'     x$gal[[i]] <- NULL
#'   }
#'   for(i in seq_along(net_attrs)) {
#'     network::set.network.attribute(x, names(net_attrs)[[i]], net_attrs[[i]])
#'   }
#'   x
#' }
#' 
#' #' @export
#' #' 
#' drop_loops <- function(x) {
#'   UseMethod("drop_loops")
#' }
#' 
#' #' @importFrom igraph simplify
#' #' @export
#' drop_loops.igraph <- function(x) {
#'   simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
#' }
#' 
#' #' @importFrom network set.network.attribute
#' #' @export
#' drop_loops.network <- function(x) {
#'   out <- set.network.attribute(x, "loops", value = FALSE)
#'   out
#' }
#' 
#' #' @export
#' has_loops <- function(x) {
#'   UseMethod("has_loops")
#' }
#' 
#' #' @importFrom igraph is.loop
#' #' @export
#' has_loops.igraph <- function(x) {
#'   any(is.loop(x))
#' }
#' 
#' #' @export
#' has_loops.network <- function(x) {
#'   el <- rep_edgelist(x)
#'   any(el[, 1] == el[, 2])
#' }
#' 
#' choose_df_class <- function(out_class) {
#'   if (out_class == "tibble") {
#'     if (requireNamespace("tibble", quietly = TRUE)) {
#'       return(tibble::as_tibble)
#'     }
#'   } 
#'   if (out_class == "data.table") {
#'     if (requireNamespace("data.table", quietly = TRUE)) {
#'       return(data.table::as.data.table)
#'     }
#'   }
#'   return(NULL)
#' }

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

# is_int <- function(x) {
  # !grepl("[^[:digit:]]", format(x,  digits = 50L, scientific = FALSE))
# }

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
  inherits(x, c("sna_net", "igraph", "network", "tbl_graph"))
}

validate_graph <- function(x) {
  if (!is_valid_graph(x)) {
    terminate("`x` must be an `sna_net`, `igraph`, `network`, or `tbl_graph`.")
  }
}

is_valid_vrt_attr <- function(x, vrt_attr) {
  !is.na(vrt_attr) && is.character(vrt_attr) && 
    is_scalar(vrt_attr) && vrt_attr %in% vrt_attr_names(x)
}

validate_vrt_attr <- function(x, vrt_attr) {
  if (!is_valid_vrt_attr(x, vrt_attr)) {
    terminate(patch('"%s" is not a valid vertex attribute. `vrt_attr` must be a scalar 
                    character corresponding to the name of an vertex attribute in `x`.',
                    edg_attr))
  }
}

is_valid_edg_attr <- function(x, edg_attr) {
  !is.na(edg_attr) && is.character(edg_attr) && 
    is_scalar(edg_attr) && edg_attr %in% edg_attr_names(x)
}

validate_edg_attr <- function(x, edg_attr) {
  if (!is_valid_edg_attr(x, edg_attr)) {
    terminate(patch('"%s" is not a valid edge attribute. `edg_attr` must be a scalar 
                    character corresponding to the name of an edge attribute in `x`.',
                    edg_attr))
  }
}

is_vrt_names_attr <- function(x) {
  x %in% c(".name", "name", "vertex.names")
}

get_vrt_names_attr <- function(x) {
  if (inherits(x, "sna_net")) {
    return(".name")
  }
  if (inherits(x, "igraph")) {
    return("name")
  } 
  if (inherits(x, "network")) {
    return("vertex.names")
  }
}


terminate <- function(..., call. = FALSE) {
  stop(txt_wrap(...), call. = call.)
}

#' @importFrom igraph set_vertex_attr vcount vertex_attr_names
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






