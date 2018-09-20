#' Construct a graph object's edgelist representation
#' 
#' `rep_as_edgelist()` standardizes edgelist representations across supported classes in 
#' that edge sources (`.ego`) and targets (`.alter`) are sorted row-wise for undirected 
#' graphs and parallel edges are never dropped from multiplex graphs.
#' 
#' @param x An `sna_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' @param use_names `logical` (default: `TRUE`) indicating whether to use vertex names in
#' the returned object. If `FALSE`, the returned object uses vertex indices.
#' @param vrt_attr `character` (default: `NULL`) indicating which vertex attribute to use 
#' in the returned object. If provided, `vrt_attr` overrides `use_names`.
#' @param weights `logical` (default: `FALSE`) indicating whether the parallel edges of 
#' multiplex graph objects should be combined into a count column named `.weight`.
#' @param leave_raw `logical` (default: `TRUE`) indicating whether to return an `edgelist`
#' object instead of a `matrix` or `data.frame`.
#' @param ignore_missing_names `logical` (default: `FALSE`) whether to throw a warning if 
#' `use_names = TRUE` but  `x` does not contain valid vertex names.
#' 
#' @return `edgelist` object, `matrix`, or `data.frame`.
#' 
#' @details 
#' The returned object contains minimally 2 columns (`.ego` and `.alter`) stored as a 
#' `matrix`.
#' * `use_names`
#'   + If `use_names` is `TRUE` (the default), `rep_as_edgelist()` checks whether `x` 
#'     contains a valid vertex attribute for vertex names. If available, this attribute is 
#'     used. If not available, the returned object uses vertex indices and a warning is 
#'     thrown (unless `ignore_missing_names = TRUE`).
#'   + Each class uses a different attribute for valid vertex names:
#'     + `igraph` and `tbl_graph` objects use a vertex attribute called `name`
#'     + `network` objects use a vertex attribute called `vertex.names`
#'     + `sna_net` objects use a vertex attribute called `.name`
#' * `weights`
#'   + If `weights` is `TRUE`, `rep_as_edgelist()` combines parallel edges to create a 
#'     third column (`.weight`).
#'     + In order to handle potentially heterogeneous data types, `rep_as_edgelist()` 
#'     stores edges in a `data.frame` instead of a `matrix`.
#'   + Ignored if `net_is_multiplex()` returns `FALSE`.
#' * `leave_raw`
#'   + If `leave_raw` is `FALSE` (the default), `rep_as_edgelist()` will return an 
#'   `edgelist` object.
#'   + `edgelist` objects store metadata describing `x` in order to facilitate operations
#'     for which a traditional edgelist requires extra information.
#'     + These metadata are stored in the following `attributes()`:
#'       + `is_directed`
#'       + `is_bipartite`
#'       + `is_multiplex`
#'       + `n_vertices`
#'       + `n_edges`
#'       + `n_actors`
#'  + If desired, an `edgelist` object can be converted using `as.data.frame()` or 
#'    `as.matrix()` in order to strip additional `attributes()`.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [igraph::as_edgelist()], [network::as.matrix.network.edgelist()]
#' 
#' @examples
#' library(snatools)
#' 
#' ig <- igraph::random.graph.game(10, 0.1) %>% 
#'   igraph::set_vertex_attr("name", value = letters[seq_len(igraph::vcount(.))]) %>% 
#'   igraph::set_vertex_attr("group", value = sample(LETTERS[1:3], igraph::vcount(.), replace = TRUE))
#' 
#' nw <- ig %>% 
#'   as_network()
#'   
#' sna_g <- ig %>% 
#'   as_sna_net()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' ig %>% rep_as_edgelist()
#' 
#' nw %>% rep_as_edgelist(use_names = FALSE)
#' 
#' tidy_g %>% rep_as_edgelist(vrt_attr = "group")
#' 
#' ig %>% rep_as_edgelist(leave_raw = TRUE)
#' 
#' @export
rep_as_edgelist <- function(x, use_names = TRUE, vrt_attr = NULL, weights = FALSE, 
                            leave_raw = FALSE, ignore_missing_names = FALSE) {
  validate_graph(x)
  if (use_names && is.null(vrt_attr)) {
    vrt_attr <- get_vrt_names_attr(x)
    if (!vrt_attr %in% vrt_attr_names(x) && !ignore_missing_names) {
      warning("\n", patch("`x`'s vertices do not have a valid name attribute. For `%s` objects, 
                    vertex names are expected to be stored in a vertex attributed called
                    `%s`. Returning an edgelist using vertex indices instead.", 
                    class(x)[[1L]], vrt_attr))
      vrt_attr <- NULL
    }
  }
  if (!is.null(vrt_attr)) {
    validate_vrt_attr(x, vrt_attr)
  }
  out <- get_el(x, vrt_attr = vrt_attr, weights = weights)
  if (leave_raw) {
    attr(out, "el_type") <- NULL
    attr(out, "vrt_attr_name") <- NULL
    attr(out, "is_weighted") <- NULL
    return(out)
  }
  out <- set_metadata_attr(out, graph = x)
  out <- set_edgelist_class(out)
  out
}

get_el <- function(x, vrt_attr = NULL, weights = FALSE) {
  UseMethod("get_el")
}

#' @importFrom igraph as_edgelist vertex_attr
get_el.igraph <- function(x, vrt_attr = NULL, weights = FALSE) {
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
  if (weights && any(duplicated.matrix(el))) {
    el <- as_weighted_el(el, el_type)
    attr(el, "is_weighted") <- "TRUE"
  } else {
    attr(el, "is_weighted") <- FALSE
  }
  attr(el, "el_type") <- el_type
  if (el_type == "vrt_attrs") {
    attr(el, "vrt_attr_name") <- vrt_attr
  }
  el
}

get_el.network <- function(x, vrt_attr = NULL, weights = FALSE) {
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
    el <- cbind(inl, outl)
    el <- t(apply(el, 1, FUN = sort))
    # if (!net_is_bipartite(x)) {
    #   el <- cbind(inl, outl)
    #   el <- t(apply(el, 1, FUN = sort))
    # } else {
    #   el <- cbind(inl, outl)
    #   el <- t(apply(el, 1, FUN = sort))
    # }
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
  if (weights && any(duplicated.matrix(el))) {
    el <- as_weighted_el(el, el_type)
    attr(el, "is_weighted") <- TRUE
  } else {
    attr(el, "is_weighted") <- FALSE
  }
  attr(el, "el_type") <- el_type
  if (el_type == "vrt_attrs") {
    attr(el, "vrt_attr_name") <- vrt_attr
  }
  el
}


get_el.tbl_graph <- function(x, vrt_attr = NULL, weights = FALSE) {
  get_el(as_igraph(x), vrt_attr = vrt_attr)
}

set_edgelist_class <- function(x) {
  if (ncol(x) == 3L) {
    class(x) <- c("edgelist", "data.frame")
  } else {
    class(x) <- c("edgelist", "matrix")
  }
  x
}

as_weighted_el <- function(x, el_type) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  out <- aggregate.data.frame(x, by = x[, c(".ego", ".alter")], length)[seq_len(ncol(x) + 1)]
  names(out)[[3]] <- ".weight"
  out
}


#' @rdname rep_as_edgelist
#' 
#' @export
is_edgelist <- function(x) {
  inherits(x, "edgelist")
}

#' @rdname rep_as_edgelist
#' 
#' @export
print.edgelist <- function(x, .nrow = 10L) {
  class(x) <- class(x)[[2L]]
  out <- head(x, .nrow)
  rownames(out) <- paste0(seq_len(nrow(out)))

  el_type <- switch(attr(x, "el_type"),
                    vrt_names = "vertex names",
                    vrt_indices = "vertex indices",
                    patch("a vertex attribute ('%s')", attr(x, "vrt_attr_name")))

  direction <- ifelse(attr(x, "is_directed"), " directed", "n undirected")
  weighted <- ifelse(attr(x, "is_weighted"), ", weighted",  "")
  bipart <- ifelse(attr(x, "is_bipartite"), ", bipartite", "")
  cat_patch("A%s%s%s edgelist with %s edges.*", 
            direction, weighted, bipart,
            format(nrow(x), big.mark = ","), initial = "# ", prefix = "## ", exdent = 2)
  cat("\n")
  print(out, quote = FALSE)
  if (nrow(out) < nrow(x)) {
    cat_patch("# ... and %s more row(s).", nrow(x) - nrow(out))
    cat("\n")
  }
  cat_patch("* Values correspond to %s.", el_type, initial = "# ", prefix = "## ")
}

#' @rdname rep_as_edgelist
#' 
#' @export
as.matrix.edgelist <- function(x, ...) {
  class(x) <- class(x)[[2L]]
  if (class(x) == "matrix") {
    mat_attrs <- c("dim", "dimnames")
    for (i in names(attributes(x))) {
      if (!i %in% mat_attrs) {
        attr(x, i) <- NULL
      }
    }
    return(x)
  }
  as.matrix(as.data.frame(x, stringsAsFactors = FALSE), ...)
}

#' @rdname rep_as_edgelist
#' 
#' @export
as.data.frame.edgelist <- function(x, stringsAsFactors = FALSE, ...) {
  class(x) <- class(x)[[2L]]
  if (class(x) == "data.frame") {
    return(x)
  }
  as.data.frame(x, stringsAsFactors = stringsAsFactors, ...)
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


