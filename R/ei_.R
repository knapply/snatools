#' E-I Index
#' 
#' Given a categorical vertex attribute describing mutually exclusive groups, the E-I 
#' index represents a ratio of external to internal ties.
#' 
#' @param x An `igraph` or `network` object.
#' @param vrt_attr `character` representing the name of a categorical vertex attribute present in `x`.
#' @param scope `character`. One of `"global"`, `"group"`, `"vertex"`, or `"full"`.
#' @param drop_loops `logical`. Whether loops should be removed prior to calculations. \cr
#'     Default: `FALSE`
#' 
#' @return One of:
#' * scalar `double`
#' * `ei_index_grp` (`data.frame`)
#' * `ei_index_vrt` (`data.frame`)
#' * `ei_index_full` (named `list`) containing a scalar `double`, an `ei_index_grp`, 
#'    and an `ei_index_vrt`.
#' See Details.
#'  
#' @details
#' \deqn{E\mbox{-}I~Index = \frac{EL-IL}{EL+IL}}
#' * Variables and  Interpretation:
#'     + \eqn{EL} is the count of external ties and \eqn{IL} is the count of internal ties.
#'     + Results closer to `-1` suggest tendencies toward homophily while results closer 
#'     to `+1` suggest tendencies toward heterophily.
#' \cr\cr
#' * `ei_index_global()` is an alias for `ei_index()` with argument `scope = "full"`.
#'     + The global E-I Index calculates the ratio of external ties to internal ties for 
#'     __all groups__.
#'     + Returns scalar `double`
#' * `ei_index_grp()` is an alias for `ei_index()` with argument `scope = "group"`.
#'     + The group E-I Index calculates the ratio of external to internal ties for __each 
#'     group__.
#'     + Returns `ei_index_grp` object, a `data.frame` with columns:
#'         + `attribute`. Group category, as present in the `vrt_attr` provided.
#'         + `external_ties`. Count of ties external to the group.
#'         + `internal_ties`. Count of ties internal to the group.
#'         + `ei_index`. Value calculated by equation above.
#' * `ei_index_vrt()` is an alias for `ei_index()` with argument `scope = "vertex"`.
#'     + The vertex E-I Index calculates the ratio of external to internal ties for __each 
#'     vertex__.
#'     + Returns `ei_index_vrt` object, a `data.frame` with columns:
#'         + `name`. Vertex names, as found in `x`'s `name` or `vertex.names` vertex attribute.
#'         + `attribute`. Group to which vertices belong, as present in the `vrt_attr` provided.
#'         + `external_ties`. Count of ties external to the group.
#'         + `internal_ties`. Count of ties internal to the group.
#'         + `ei_index`. Value calculated by equation above.
#' * `ei_index()` with argument `scope = "full"` calculates global, group, and vertex E-I
#' Indices.
#'     + Returns `ei_index_full` object, a `list` with elements (in formats described above):
#'         + `global`
#'         + `by_group`
#'         + `by_vertex`
#' 
#' @references Krackhardt, David, and Robert N. Stern. "Informal Networks and 
#' Organizational Crises: An Experimental Simulation." Social Psychology Quarterly 51, no.
#'  2 (1988): 123-40. \url{http://www.jstor.org/stable/2786835}.
#'  
#' @seealso [ei_index_global_permute()], [rep_mixing_matrix()], [network::mixingmatrix()]
#'  
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples
#' library(snatools)
#' 
#' data("samplk", package = "ergm")
#' 
#' ei_index(samplk1, vrt_attr = "group", scope = "full")
#' 
#' (ei_global <- ei_index_global(samplk1, vrt_attr = "group"))
#' 
#' (ei_by_group <- ei_index_grp(samplk1, vrt_attr = "group"))
#' 
#' autoplot(ei_by_group)
#' 
#' (ei_by_vertex <- ei_index_vrt(samplk1, vrt_attr = "group"))
#' 
#' autoplot(ei_by_vertex)
#' 
#' @export
ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex", "full"), drop_loops = FALSE) {
  scope <- match.arg(scope, c("global", "group", "vertex", "full"), several.ok = FALSE)
  
  if(drop_loops) {
    x <- drop_loops(x)
  }
  if(scope %in% c("global", "full")) {
    ei_global <- ei_index_global(x, vrt_attr)
    if(scope == "global") {
      return(ei_global)
    }
  }
  if(scope %in% c("group", "full")) {
    ei_grp <- ei_index_grp(x, vrt_attr)
    if(scope == "group") {
      return(ei_grp)
    }
  }
  if(scope %in% c("vertex", "full")) {
    ei_vrt <- ei_index_vrt(x, vrt_attr)
    if(scope == "vertex") {
      return(ei_vrt)
    }
  }
  
  out <- list(global = ei_global,
              by_group = ei_grp,
              by_vertex = ei_vrt)
  class(out) <- "ei_index_full"
  
  out
}


#' Global E-I Index
#' 
#' Calculate a network's global E-I Index from various representations.
#' 
#' @param x `igraph`, `network`, `tbl_graph`, `attr_el`, `attr_adj_mat`, or `mixing_matrix`
#' @param vrt_attr `character` representing the name of a categorical vertex attribute
#'        present in `x`.
#' @param drop_loops `logical`. Whether loops should be removed prior to calculations. \cr
#'        Default: `FALSE`
#' 
#' @return A scalar `double`.
#' 
#' @seealso [ei_index()]. [rep_attr_el()], [rep_attr_adj_mat()], [rep_mixing_matrix()]
#' 
#' @examples 
#' library(snatools)
#' 
#' # igraph ==============================================================================
#' data("UKfaculty", package = "igraphdata")
#' 
#' ei_index_global(UKfaculty, vrt_attr = "Group")
#' 
#' # network =============================================================================
#' data("samplk", package = "ergm")
#' 
#' ei_index(samplk1, vrt_attr = "group")
#' 
#' # attr_adj_mat ========================================================================
#' (attr_adj_mat <- rep_attr_adj_mat(samplk1, vrt_attr = "group"))
#' ei_index_global(attr_adj_mat)
#' 
#' # attr_el =============================================================================
#' (attr_el <- rep_attr_el(attr_adj_mat)) %>% head()
#' ei_index_global(attr_el)
#' 
#' # mixing_matrix =======================================================================
#' (mix_mat <- rep_mixing_matrix(samplk1, vrt_attr = "group"))
#' ei_index_global(mix_mat)
#' 
#' # tbl_graph ===========================================================================
#' (tbl_g <- tidygraph::as_tbl_graph(UKfaculty))
#' ei_index_global(tbl_g, "Group")
#' 
#' @export
#' 
ei_index_global <- function(x, vrt_attr, ...) {
  UseMethod("ei_index_global")
}


#' @rdname ei_index_global
#' 
#' @export
#' 
ei_index_global.igraph <- function(x, vrt_attr, drop_loops = FALSE) { # fast E-I
  if(!vrt_attr %in% vrt_get_attr_names(x)) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  if(drop_loops) {
    x <- igraph::simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
  }
  attrs <- vrt_get_attr(x, vrt_attr)
  el <- rep_edgelist(x)
  
  attr_el <- matrix(attrs[el], ncol = 2)
  
  class(attr_el) <- "attr_el"
  
  ei_index_global(attr_el)
}

#' @rdname ei_index_global
#' 
#' @export
#' 
ei_index_global.network <- function(x, vrt_attr, drop_loops = FALSE) { # fast E-I
  if(!vrt_attr %in% vrt_get_attr_names(x)) {
    stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
  }
  if(drop_loops) {
    network::set.network.attribute(x, "loops", value = FALSE)
  }
  attrs <- vrt_get_attr(x, vrt_attr)
  el <- rep_edgelist(x)
  
  attr_el <- matrix(attrs[el], ncol = 2)
  class(attr_el) <- "attr_el"
  
  ei_index_global(attr_el)
}

#' @rdname ei_index_global
#' 
#' @export
#' 
ei_index_global.tbl_graph <- function(x, vrt_attr, ...) {
  ei_index_global(as_igraph(x), vrt_attr, ...)
}

#' @rdname ei_index_global
#' 
#' @export
#' 
ei_index_global.attr_el <- function(attr_el) {
  n <- nrow(attr_el)
  internal <- length(which(attr_el[, 1] == attr_el[, 2]))
  external <- n - internal
  
  (external - internal) / n
}

#' @rdname ei_index_global
#' 
#' @export
#' 
ei_index_global.attr_adj_mat <- function(x) {
  attr_el <- rep_attr_el(x)
  ei_index_global(attr_el)
}

#' @rdname ei_index_global
#' 
#' @export
#' 
ei_index_global.mixing_matrix <- function(mixing_matrix) {
  mixing_density <- mixing_matrix / sum(mixing_matrix)
  internal <- sum(diag(mixing_density))
  diag(mixing_density) <- NA_integer_
  external <- sum(mixing_density, na.rm = TRUE)
  
  (external - internal) / (external + internal)
}


#' @rdname ei_index
#' 
#' E-I Index of Each Vertex
#' 
#' @export
#' 
ei_index_vrt <- function(x, vrt_attr, drop_loops = FALSE) {
  if(drop_loops) {
    x <- drop_loops(x)
  }
  vrt_names <- vrt_get_names(x)
  attrs <- vrt_get_attrs(x)[[vrt_attr]]
  attr_mat <- rep_attr_adj_mat(x, vrt_attr)
  rownames(attr_mat) <- vrt_names
  mix_mat_names <- t(rowsum(t(attr_mat), group = colnames(attr_mat), na.rm = TRUE))
  
  mix_mat_for_external <- `rownames<-`(mix_mat_names, attrs)
  matches <- match(rownames(mix_mat_for_external), colnames(mix_mat_for_external))
  match_mat <- cbind(seq_along(matches), matches)
  mix_mat_for_external[match_mat] <- NA_integer_
  external <- rowSums(mix_mat_for_external, na.rm = TRUE)
  names(external) <- vrt_names

  mix_mat_for_internal <- `rownames<-`(mix_mat_names, attrs)
  mix_mat_for_internal[!is.na(mix_mat_for_external)] <- NA_integer_
  internal <- rowSums(mix_mat_for_internal, na.rm = TRUE)
  names(internal) <- vrt_names
  
  ei <- (external - internal) / (external + internal)
  
  out <- data.frame(name = vrt_names,
                    attribute = attrs,
                    external_ties = external,
                    internal_ties = internal,
                    ei_index = ei,
                    stringsAsFactors = FALSE)
  out$ei_index[is.nan(out$ei_index)] <- 0
  rownames(out) <- NULL
  class(out) <- c("ei_index_vrt", "data.frame")
  
  out
}

#' @export
#' 
autoplot.ei_index_vrt <- function(x) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  x$name <- factor(x$name, levels = rev(x$name))
  out <- ggplot2::ggplot(x)
  out <- out + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ei_index, 
                                                  y = name, yend = name,color = ei_index), 
                                     alpha = 0.5, show.legend = FALSE)
  out <- out + ggplot2::geom_point(ggplot2::aes(x = 0, y = name), size = 4, 
                                   color = "lightgray")
  out <- out + ggplot2::geom_point(ggplot2::aes(x = ei_index, y = name, fill = ei_index), 
                                   size = 4, shape = 21, show.legend = FALSE)
  out <- out + ggplot2::scale_color_distiller(palette = "RdBu", direction = 1)
  out <- out + ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1)
  out <- out + ggplot2::labs(x = "E-I Index", y = "Vertex Name", 
                             title = "E-I Index, Vertices")
  out <- out + theme_sna()
  out <- out + scale_x_ratio()
  
  out
}


#' @rdname ei_index
#' 
#' E-I Index of Each Group
#' 
#' @export
#' 
ei_index_grp <- function(x, vrt_attr, drop_loops = FALSE) {
  if(drop_loops) {
    x <- drop_loops(x)
  }
  mix_mat <- rep_mixing_matrix(x, vrt_attr)
  internal <- diag(mix_mat)
  diag(mix_mat) <- NA_integer_
  external <- rowSums(mix_mat, na.rm = TRUE)
  
  ei <- (external - internal) / (external + internal)
  
  out <- data.frame(attribute = rownames(mix_mat),
                    external_ties = external,
                    internal_ties = internal,
                    ei_index = ei, 
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  class(out) <- c("ei_index_grp", "data.frame")
  
  out
}


#' @export
#' 
autoplot.ei_index_grp <- function(x) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  out <- ggplot2::ggplot(x)
  out <- out + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ei_index, 
                                                  y = attribute, yend = attribute, 
                                                  color = ei_index), 
                                     alpha = 0.5, show.legend = FALSE)
  out <- out + ggplot2::geom_point(ggplot2::aes(x = 0, y = attribute),
                                   size = 4, color = "lightgray")
  out <- out + ggplot2::geom_point(ggplot2::aes(x = ei_index, y = attribute, 
                                                fill = ei_index), 
                                   size = 4, shape = 21, color = "lightgray", 
                                   show.legend = FALSE)
  out <- out + ggplot2::scale_color_distiller(palette = "RdBu", direction = 1)
  out <- out + ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1)
  out <- out + ggplot2::labs(x = "E-I Index", y = "Attribute Category",
                             title = "E-I Index, Groups")
  out <- out + theme_sna()
  out <- out + scale_x_ratio()
  
  out
}



#' E-I Index Permutation Test
#' 
#' @export
#' 
#' @examples
#' library(snatools)
#' 
#' data("samplk", package = "ergm")
#' 
#' res1 <- ei_index_global_permute(samplk1, "cloisterville")
#' 
#' res1
#' 
#' autoplot(res1)
#' 
#' res2 <- ei_index_global_permute(samplk1, "group")
#' 
#' res2
#' 
#' autoplot(res2)
#' 
ei_index_global_permute <- function(x, vrt_attr, iterations = 1000L, diagonal = FALSE) {
  if(!class(x) %in% c("igraph", "network")) {
    stop("`x` must be of class `igraph` or `network`.", call. = FALSE)
  }
  observed_ei <- ei_index_global(x, vrt_attr)            # value test
  attr_adj_mat <- rep_attr_adj_mat(x, vrt_attr)    # matrix to permute
  permuted_eis <- vector("double", iterations)        # initialize storage vector
  if(class(x) == "network") {
    directed <- x$gal$directed
  }
  if(class(x) == "igraph") {
    directed <- igraph::is_directed(x)
  }
  if(directed) {
    for(i in seq_len(iterations)) {
      permuted_matrix <- snatools:::permute_matrix(attr_adj_mat, # permute the matrix
                                                   out_class = "attr_adj_mat")
      if(!diagonal) {
        diag(permuted_matrix) <- NA_integer_                     # remove the diagonal
      }
      permuted_attr_el <- rep_attr_el(permuted_matrix)        # convert to el for fast EI
      permuted_eis[[i]] <- ei_index_global(permuted_attr_el)        # calc EI, add to `permuted_eis`
    }
  } else { # for undirected networks: remove the upper triangle
    for(i in seq_len(iterations)) {
      permuted_matrix <- snatools:::permute_matrix(attr_adj_mat, out_class = "attr_adj_mat")
      permuted_matrix[upper.tri(permuted_matrix, diag = !diagonal)] <- NA_integer_
      permuted_attr_el <- rep_attr_el(permuted_matrix)
      permuted_eis[[i]] <- ei_index_global(permuted_attr_el)
    }
  }
  out <- list(vrt_attr = vrt_attr,
              observed_ei = observed_ei,
              iterations = iterations,
              permuted_eis = permuted_eis,
              n_greater = length(which(permuted_eis >= observed_ei)),
              prop_greater = mean(as.numeric(permuted_eis >= observed_ei)),
              n_lesser = length(which(permuted_eis < observed_ei)),
              prop_lesser = mean(as.numeric(permuted_eis < observed_ei)))
  class(out) <- "ei_index_global_permute"

  out
}

#' @export
#' 
print.ei_index_global_permute <- function(x) {
  cat("Target Attribute:                 ", x$vrt_attr, "\n")
  cat("\n")
  cat("Observed E-I Index:               ", x$observed_ei, "\n")
  cat("\n")
  cat("Iterations:                       ", x$iterations, "\n")
  cat("\n")
  cat("# Permutations >= Observed:       ", x$n_greater, " (", 
      round(x$prop_greater * 100, 2), "%)\n", sep = "")
  cat("# Permutations < Observed:        ", x$n_lesser, " (", 
      round(x$prop_lesser * 100, 2), "%)\n", sep = "")
}

#' @export
#' 
autoplot.ei_index_global_permute <- function(x, leave_bare = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  df <- data.frame(perm_ei = x$permuted_eis)
  out <- ggplot2::ggplot(df)
  out <- out + ggplot2::stat_density(ggplot2::aes(perm_ei), fill = "orange", alpha = 0.5)
  out <- out + ggplot2::geom_vline(ggplot2::aes(xintercept = x$observed_ei, 
                                                color = "Observed E-I Index"))
  out <- out + ggplot2::guides(color = ggplot2::guide_legend(NULL))
  out <- out + ggplot2::labs(x = paste("E-I Indices of Permuted Matrices"), y = "Density",
                             title = "E-I Index", 
                             subtitle = paste("Observed E-I:", round(x$observed_ei, 3)),
                             caption = paste("Iterations:", scales::comma(x$iterations)))
  out <- out + theme_sna()
  out <- out + scale_x_ratio()
  
  out
}




