#' E-I Index
#' 
#' Given a categorical vertex attribute describing mutually exclusive groups, the E-I 
#' index represents a ratio of external to internal ties.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param vrt_attr `character` representing the name of a categorical vertex attribute present in `x`.
#' @param scope `character`. One of `"global"`, `"group"`, `"vertex"`, or `"full"`.
#' @param drop_loops `logical`. Whether loops should be removed prior to calculations. \cr
#'     Default: `FALSE`
#'  
#' @details
#' \deqn{E\mbox{-}I~Index = \frac{EL-IL}{EL+IL}}
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
#' data(sampson_monastery)
#' 
#' sampson_monastery %>% 
#'   edg_filter(time == 1) %>% 
#'   ei_index(vrt_attr = "faction", scope = "global") %>% 
#'   sna_plot()
#'   
#' sampson_monastery %>% 
#'   as_igraph() %>% 
#'   edg_filter(time == 2) %>% 
#'   ei_index(vrt_attr = "faction", scope = "global")
#'   
#' sampson_monastery %>% 
#'   as_network() %>% 
#'   edg_filter(time == 3, relation == "liking") %>% 
#'   ei_index(vrt_attr = "faction", scope = "global")
#'   
#' sampson_monastery %>% 
#'   tidygraph::as_tbl_graph() %>% 
#'   edg_filter(time == 3, relation == "disliking") %>% 
#'   ei_index(vrt_attr = "faction", scope = "global")
#'   
#' @export
ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex"),
                     drop_loops = FALSE) {
  validate_graph(x)
  
  scope <- match.arg(scope, c("global", "groups", "vertices"))
  switch(scope,
         global = ei_index_global(x, vrt_attr, drop_loops = drop_loops),
         group = ei_index_group(x, vrt_attr, drop_loops = drop_loops),
         vertex = ei_index_vertex(x, vrt_attr, drop_loops = drop_loops))
}

ei_index_global <- function(x, vrt_attr, drop_loops = FALSE) {
  validate_vrt_attr(x, vrt_attr)
  el <- rep_as_edgelist(x, vrt_attr = vrt_attr, weights = TRUE, leave_raw = TRUE)
  n <- nrow(el)
  external <- sum(el[el[[".ego"]] != el[[".alter"]], ][[".weight"]])
  internal <- sum(el[el[[".ego"]] == el[[".alter"]], ][[".weight"]])
  out <- (external - internal) / (external + internal)
  attr(out, "vrt_attr_name") <- vrt_attr
  class(out) <- "ei_index_global"
  out
}

#' @rdname ei_index
#' 
#' @export
print.ei_index_global <- function(x, ...) {
  cat("# An ei_index_global object.")
  cat("\n")
  cat_patch("# vertex attribute: %s", attr(x, "vrt_attr_name"))
  cat("\n")
  cat("\n")
  print(`attributes<-`(x, NULL))
}

# ei_index_global.igraph <- function(x, vrt_attr, drop_loops = FALSE) {
#   if (vrt_attr == ".name") {
#     terminate('".name" is not a valid vertex attribute.')
#   }
#   el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
#   if(drop_loops && net_has_loops(x)) {
#     el <- unique.matrix(el)
#   }
#   n_edges <- nrow(el)
#   external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
#   internal <- n_edges - external
#   
#   (external - internal) / n_edges
# }
# 
# ei_index_global.network <- function(x, vrt_attr, drop_loops = FALSE) {
#   if (vrt_attr == ".name") {
#     terminate('".name" is not a valid vertex attribute.')
#   }
#   el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
#   if(drop_loops && net_has_loops(x)) {
#     el <- unique.matrix(el)
#   }
#   n_edges <- nrow(el)
#   external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
#   internal <- n_edges - external
#   
#   (external - internal) / n_edges
# }
# 
# ei_index_global.adj_matrix <- function(x, drop_loops = FALSE) {
#   # if (attr(x, "vrt_attr") == ".name") {
#   #   terminate('".name" is not a valid vertex attribute.')
#   # }
#   el <- rep_as_edgelist(x, leave_raw = TRUE)
#   if(drop_loops && net_has_loops(x)) {
#     el <- unique.matrix(el)
#   }
#   n_edges <- nrow(el)
#   external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
#   internal <- n_edges - external
#   
#   (external - internal) / n_edges
# }

ei_index_group <- function(x, vrt_attr, drop_loops = FALSE) {
  mix_mat <- rep_as_mixing_matrix(x, vrt_attr, drop_loops = drop_loops)
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

ei_index_vertex <- function(x, vrt_attr, drop_loops = FALSE) {
  stop("Not yet implemented.")
  x
  vrt_attr
  drop_loops
}


#' @rdname ei_index
#' 
#' @export
sna_plot.ei_index_global <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  ei <- object
  vrt_attr <- attr(ei, "vrt_attr_name")
  ei <- unclass(ei)
  color_pal <- ifelse(ei < 0, colorRampPalette(c("red", "gray")), 
                      colorRampPalette(c("gray", "blue")))
  color <- color_pal(10)[[ifelse(ei < 0, round((ei + 1) * 10), round(ei * 10))]]
  out <- ggplot2::ggplot()
  out <- out + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ei, 
                                                  y = 0.5, yend = 0.5))
  out <- out + ggplot2::geom_point(ggplot2::aes(x = 0, y = 0.5), size = 8,
                                   color = "lightgray")
  out <- out + ggplot2::geom_point(ggplot2::aes(x = ei, y = 0.5), fill = color, 
                                   size = 12, shape = 21, show.legend = FALSE)
  out <- out + ggplot2::geom_text(ggplot2::aes(x = 0.75, y = 0, 
                                               label = "More Heterophilous"))
  out <- out + ggplot2::geom_text(ggplot2::aes(x = -0.75, y = 0, 
                                               label = "More Homophilous"))
  out <- out + ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1)
  out <- out + ggplot2::theme_minimal(base_family = "serif")
  out <- out + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                              panel.grid.minor.x = ggplot2::element_blank(),
                              axis.text.y = ggplot2::element_blank())
  out <- out + ggplot2::labs(x = "E-I Index", y = NULL, 
                             title = patch("Global E-I Index: %s",
                                           round(ei, 3)),
                             subtitle = patch("Vertex Attribute: %s",
                                              txt_to_title(vrt_attr)))
  out <- out + ggplot2::scale_x_continuous(limits = c(-1, 1))
  out <- out + ggplot2::scale_y_continuous(limits = c(0, 1))
  out
}

# #' @rdname ei_index
# #' 
# #' @export
# autoplot.ei_index_grp <- function(x) {
#   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#     stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
#   }
#   x$name <- factor(x$name, levels = rev(x$name))
#   out <- ggplot2::ggplot(x)
#   out <- out + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ei_index, 
#                                                   y = name, yend = name,color = ei_index), 
#                                      alpha = 0.5, show.legend = FALSE)
#   out <- out + ggplot2::geom_point(ggplot2::aes(x = 0, y = name), size = 4, 
#                                    color = "lightgray")
#   out <- out + ggplot2::geom_point(ggplot2::aes(x = ei_index, y = name, fill = ei_index), 
#                                    size = 4, shape = 21, show.legend = FALSE)
#   out <- out + ggplot2::scale_color_distiller(palette = "RdBu", direction = 1)
#   out <- out + ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1)
#   out <- out + ggplot2::labs(x = "E-I Index", y = "Vertex Name", 
#                              title = "E-I Index, Vertices")
#   out <- out + theme_sna()
#   out <- out + scale_x_ratio()
#   
#   out
# }


# permute_matrix <- function(adj_matrix, test_fun, ...) {
#   n <- seq_len(nrow(adj_matrix))
#   force(test_fun)
# 
#   function() {
#     rownames(adj_matrix) <- rownames(adj_matrix)[sample(n)]
#     colnames(adj_matrix) <- rownames(adj_matrix)
#     test_fun(adj_matrix, ...)
#   }
# }

# adj_mat_tester <- igraph::graph("Zachary") %>% 
#   igraph::set_vertex_attr("name", value = c(letters, LETTERS)[seq_len(igraph::vcount(.))]) %>% 
#   igraph::as_adjacency_matrix(sparse = FALSE) %>% 
#   `class<-`(c("adj_matrix", "matrix"))
  
# adj_mat_tester
# permute_matrix(force)
# 
# adj_mat_permuter()
# 
# data(sampson, package = "ergm")

# rep_as_adj_matrix.igraph <- function(x) {
  # 
# }

# rep_as_edgelist.adj_matrix <- function(x) {
#   out <- as.data.frame.table(x, responseName = "n", 
#                              stringsAsFactors = FALSE)
#   out <- out[out[["n"]] > 0L, ]
#   out[["n"]] <- NULL
#   colnames(out) <- c(".ego", ".alter")
#   rownames(out) <- NULL
#   as_edge_data_frame(out)
# }
# 
# library(tidyverse)
# samplike %>% 
#   # as_igraph() %>% 
#   # igraph::as_adjacency_matrix()
#   network::set.vertex.attribute("vertex.names", 
#                                 network::get.vertex.attribute(samplike, "group")) %>% 
#   network::as.matrix.network() %>% 
#   permute_matrix() %>% 
#   rep_as_edgelist.adj_matrix() %>% 
#   network::network() %>% 
#   ei_index_global("group")
#   
#   
#   
#   permute_matrix() %>% 
#   as_tibble() %>% 
#   gather(var, val)
#   igraph::graph_from_edgelist() %>% 
  
