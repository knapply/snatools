#' E-I Index
#' 
#' Given a categorical vertex attribute describing mutually exclusive groups, the E-I 
#' index represents a ratio of external to internal ties.
#' 
#' @template graph-param
#' @template vrt_attr-param
#' @param scope `character` (default: `"global"`) indicating the target scope. 
#' Choices: `"global"`, `"group"`, or `"vertex"`.
#' 
#' @template bknapp-author
#' 
#' @export
ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex")) {
  validate_args(x, vrt_attr, validate_graph = TRUE)
  scope <- match.arg(scope, c("global", "groups", "vertices"))
  switch(scope,
         global = ei_global(x, vrt_attr, loops = loops),
         group = ei_group(x, vrt_attr),
         vertex = ei_vertex(x, vrt))
}

ei_global <- function(x, vrt_attr) {
  attrs <- vrt_get_attr(x, vrt_attr)
  el <- matrix(attrs[get_el(x)], ncol = 2L)
  # if (drop_loops && net_has_loops(x)) {
    # el <- unique.matrix(el)
  # }
  n_edges <- nrow(el)
  external <- length(el[, 1L][el[, 1L] != el[, 2L]])
  internal <- n_edges - external
  (external - internal) / n_edges
}

#' @importFrom tibble tibble
ei_group <- function(x, vrt_attr) {
  mix_mat <- rep_as_mixing_matrix(x, vrt_attr)
  internal <- diag(mix_mat)
  diag(mix_mat) <- NA_integer_
  external <- `storage.mode<-`(rowSums(mix_mat, na.rm = TRUE), "integer")
  ei <- (external - internal) / (external + internal)
  tibble::tibble(attribute = rownames(mix_mat),
                 external_ties = external,
                 internal_ties = internal,
                 ei_index = ei)
}

#' @importFrom tibble tibble
ei_vertex <- function(x, vrt_attr) {
  vrt_names <- vrt_get_names(x)
  adj_list <- rep_as_adj_list(x, vrt_attr = vrt_attr)
  out <- mapply(function(ego, alters) {
    n_edges <- length(alters)
    external <- length(alters[alters != ego])
    internal <- n_edges - external
    (external - internal) / (external + internal)
    }, names(adj_list), adj_list, USE.NAMES = FALSE)
  `names<-`(out, vrt_names)
}



