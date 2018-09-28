#' Construct a graph object's adjacency matrix representation.
#' 
#' @param x An `bridge_net`, `igraph`, `network`, or`tbl_graph`, or `edgelist` object.
#' @param use_names `logical` (default: `TRUE`) indicating whether to use vertex names for
#' row and column names.
#' @param vrt_attr `character` (default: `NULL`) indicating which vertex attribute to use 
#' for row and column names. If provided, `vrt_attr` overrides `use_names`.`
#' @param edg_attr `character` (default: `NULL`) indicating which edge attribute to use 
#' to fill the matrix. If provided, missing edges will be filled with `NA`.
#' @param leave_raw `logical` (default: `FALSE`) indicating whether to return a raw 
#' `matrix` instead of an `adj_matrix` object.
#' @param ignore_missing_names `logical` (default: `TRUE`) whether to throw a warning if 
#' `use_names = TRUE` but  `x` does not contain valid vertex names.
#' 
#' @return An `adj_matrix` or `matrix`.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#'   
#' # sampson_monastery %>% 
#' # edg_filter(time == 1) %>% 
#' #   rep_as_adj_matrix()
#' 
#' sampson_monastery %>% 
#'   edg_filter(time == 1) %>% 
#'   as_igraph() %>% 
#'   rep_as_adj_matrix(vrt_attr = "status")
#'   
#' sampson_monastery %>% 
#'   edg_filter(time == 2) %>% 
#'   as_network() %>% 
#'   rep_as_adj_matrix(edg_attr = "weight")
#'   
#' sampson_monastery %>% 
#'   edg_filter(time == 3, relation == "blame") %>% 
#'   tidygraph::as_tbl_graph() %>% 
#'   rep_as_adj_matrix(vrt_attr = "faction")
#' 
#' @export
rep_as_adj_matrix <- function(x, use_names = TRUE, vrt_attr = NULL, edg_attr = NULL,
                              leave_raw = FALSE, ignore_missing_names = TRUE) {
  validate_graph(x)
  if (net_is_multiplex(x)) {
    terminate(patch("`%s` is multiplex. Filter `%s`'s edges before calling 
                    `rep_as_adj_matrix()`", 
                    deparse(substitute(x)), deparse(substitute(x))))
  }
  if (use_names && is.null(vrt_attr)) {
    vrt_attr <- get_vrt_names_attr(x)
    if (!is_valid_vrt_attr(x, vrt_attr)) {
      if (!ignore_missing_names) {
        cat("\n")
        warning(patch("`x`'s vertices do not have a valid name attribute. For `%s` 
                      objects, vertex names are expected to be stored in a vertex 
                      attributed called `%s`. Returning an edgelist using vertex indices
                      instead.", class(x)[[1L]], vrt_attr))
      }
      vrt_attr <- NULL
      adj_mat_type <- "vrt_indices"
    } else {
      adj_mat_type <- "vrt_names"
    }
  } else if (!is.null(vrt_attr)) {
      validate_vrt_attr(x, vrt_attr)
      adj_mat_type <- "vrt_attrs"
  } else if (!use_names && is.null(vrt_attr)) {
      adj_mat_type <- "vrt_indices"
  }
  # if (!is.null(edg_attr)) {
  #   validate_edg_attr(x, edg_attr)
  # }
  out <- get_adj_mat(x, vrt_attr = vrt_attr, edg_attr = edg_attr)
  if (leave_raw) {
    return(as_matrix(out))
  }
  out <- set_metadata_attr(out, x)
  attr(out, "adj_mat_type") <- adj_mat_type
  if (adj_mat_type == "vrt_attrs") {
    attr(out, "vrt_attr_name") <- vrt_attr
  }
  if (!is.null(edg_attr)) {
    attr(out, "edg_attr_name") <- edg_attr
  }
  class(out) <- c("adj_matrix", "matrix")
  out
}

get_adj_mat <- function(x, vrt_attr = NULL, edg_attr = NULL) {
  n_vertices <- net_count_vertices(x)
  el <- rep_as_edgelist(x, use_names = FALSE, leave_raw = TRUE)
  if (!is.null(edg_attr)) {
    fill <- edg_get_attr(x, edg_attr)
    init <- rep(as(NA, typeof(fill)), n_vertices * n_vertices)
  } else {
    fill <- 1L
    init <- vector(typeof(fill), n_vertices * n_vertices)
  }
  if (is.null(vrt_attr)) {
    out <- matrix(init, nrow = n_vertices, ncol = n_vertices)
  } else {
    dim_names <- vrt_get_attr(x, vrt_attr)
    out <- matrix(init,nrow = n_vertices, ncol = n_vertices, 
                  dimnames = list(dim_names, dim_names))
  }
  out[el] <- fill
  out
}

#' @rdname rep_as_adj_matrix
#' 
#' @export
print.adj_matrix <- function(x) {
  n_col <- ncol(x)
  cat_patch("# An adj_matrix: %sx%s", nrow(x), n_col)
  cat("\n")
  if (attr(x, "adj_mat_type") != "vrt_indices") {
    cat_patch("# Truncating names row/column names.")
    cat("\n")
  }
  if (is.character(rownames(x))) {
    rownames(x) <- paste0(txt_extract(rownames(x), "^.{3}"), ".")
  }
  if (is.character(colnames(x))) {
    colnames(x) <- txt_extract(colnames(x), "^.{1}")
  }
  print(as_matrix(x))
  cat_patch("* Columns/Rows correspond to %s.", attr(x, "adj_mat_type"),
            initial = "# ", prefix = "## ")
  cat("\n")
  if (!is.null(attr(x, "edg_attr_name"))) {
    cat_patch("* Matrix values correspond to an edge attribute (`%s`).",
              attr(x, "edg_attr_name"),
              initial = "# ", prefix = "## ")
    cat("\n")
  }
  invisible(x)
}

as.matrix.adj_matrix <- function(x) {
  as_matrix(x)
}


# # @importFrom Matrix forceSymmetric isSymmetric Matrix sparseMatrix
# # @export
# old_rep_as_adj_matrix <- function(x, use_names = TRUE, vrt_attr = NULL, sparse = TRUE) {
#   validate_graph(x)
#   if (net_is_multiplex(x)) {
#     terminate(patch("`%s` is multiplex. Filter `%s`'s edges before calling 
#                     `rep_as_adj_matrix()`", 
#                     deparse(substitute(x)), deparse(substitute(x))))
#   }
#   if (use_names && is.null(vrt_attr)) {
#     vrt_attr <- get_vrt_names_attr(x)
#     adj_mat_type <- "vrt_names"
#   }
#   if (!is.null(vrt_attr)) {
#     validate_vrt_attr(x, vrt_attr)
#     adj_mat_type <- "vrt_attrs"
#     dim_names <- vrt_get_attr(x, vrt_attr)
#   } else {
#     adj_mat_type <- "vrt_indices"
#     dim_names <- NULL
#   }
#   n_vertices <- net_count_vertices(x)
#   el <- rep_as_edgelist(x, use_names = FALSE, leave_raw = TRUE)
#   if (sparse) {
#     fill <- rep(1L, nrow(el))
#     out <- sparseMatrix(i = pmin.int(el[, ".ego"], el[, ".alter"]),
#                         j = pmax.int(el[, 1], el[, 2]),
#                         x = fill,
#                         symmetric = !net_is_directed(x),
#                         dims = c(n_vertices, n_vertices))
#   } else {
#     out <- Matrix(integer(n_vertices^2), nrow = n_vertices, ncol = n_vertices,
#                   sparse = FALSE)
#     out[el] <- 1L
#     if (!net_is_directed(x) && !isSymmetric(out)) {
#       out <- forceSymmetric(out)
#     }
#   }
#   if (!is.null(dim_names)) {
#     dimnames(out) <- list(dim_names, dim_names)
#   }
#   out <- as.matrix(out)
#   attr(out, "adj_mat_type") <- adj_mat_type
#   if (adj_mat_type == "vrt_attrs") {
#     attr(out, "vrt_attr_name") <- vrt_attr
#   }
#   out <- set_metadata_attr(out, x)
#   class(out) <- c("adj_matrix", "matrix")
#   out
# }
