#' E-I Index
#' 
#' Given a categorical vertex attribute describing mutually exclusive groups, the E-I 
#' index represents a ratio of external to internal ties.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param vrt_attr `character` representing the name of a categorical vertex attribute present in `x`.
#' @param scope `character`. One of `"global"`, `"group"`, `"vertex"`, or `"full"`.
#' @param drop_loops `logical` (default: `TRUE`) indicating whether loops should be 
#' removed prior to calculations.
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
#' library(ggplot2)
#' 
#' data(sampson_monastery)
#' 
#' sampson_monastery %>% 
#'   edg_filter(time == 1) %>% 
#'   ei_index(vrt_attr = "faction", scope = "global")
#'   
#' sampson_monastery %>% 
#'   edg_filter(time == 3, positive_relation) %>% 
#'   ei_index(vrt_attr = "faction", scope = "group") %>% 
#'   autoplot()
#'   
#' @export
ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex"),
                     drop_loops = TRUE) {
  # validate_graph(x)
  scope <- match.arg(scope, c("global", "group", "vertices"))
  switch(scope,
         global = ei_index_global(x, vrt_attr, drop_loops = drop_loops),
         group = ei_index_group(x, vrt_attr, drop_loops = drop_loops),
         vertex = ei_index_vertex(x, vrt_attr, drop_loops = drop_loops))
}

ei_index_global <- function(x, vrt_attr, drop_loops = TRUE) {
  validate_vrt_attr(x, vrt_attr)
  el <- rep_as_edgelist(x, vrt_attr = vrt_attr, weights = TRUE, leave_raw = TRUE)
  n <- nrow(el)
  external <- sum(el[el[[".ego"]] != el[[".alter"]], ][[".weight"]])
  internal <- sum(el[el[[".ego"]] == el[[".alter"]], ][[".weight"]])
  out <- (external - internal) / (external + internal)
  attr(out, "vrt_attr_name") <- vrt_attr
  class(out) <- "ei_global"
  out
}


ei_index_group <- function(x, vrt_attr, drop_loops = FALSE) {
  el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
  attrs <- sort(unique(`dim<-`(el, NULL)))
  mix_mat <- table(.ego = factor(el[, ".ego"], levels = attrs),
                   .alter = factor(el[, ".alter"], levels = attrs), dnn = NULL)
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
  class(out) <- c("ei_group", "data.frame")
  out
}



#' @rdname ei_index
#' 
#' @export
print.ei_global <- function(x, ...) {
  cat("# An ei_index_global object.")
  cat("\n")
  cat_patch("# vertex attribute: %s", attr(x, "vrt_attr_name"))
  cat("\n")
  cat("\n")
  print(`attributes<-`(x, NULL))
}

#' @rdname ei_index
#' 
#' @export
autoplot.ei_group <- function(object, ...) {
  x <- object
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



# ei_index_group_old <- function(x, vrt_attr, drop_loops = TRUE) {
#   mix_mat <- rep_as_mixing_matrix(x, vrt_attr, drop_loops = drop_loops)
#   internal <- diag(mix_mat)
#   diag(mix_mat) <- NA_integer_
#   external <- rowSums(mix_mat, na.rm = TRUE)
#   ei <- (external - internal) / (external + internal)
#   out <- data.frame(attribute = rownames(mix_mat),
#                     external_ties = external,
#                     internal_ties = internal,
#                     ei_index = ei, 
#                     stringsAsFactors = FALSE)
#   rownames(out) <- NULL
#   class(out) <- c("ei_index_grp", "data.frame")
#   out
# }

#' 
#' x <- sampson_monastery %>% edg_filter(time == 1)
#' 
#' ei_group_permute <- function(x, vrt_attr, times = 10L, drop_loops = FALSE) {
#'   test_stat <- ei_index_group2(x, vrt_attr = vrt_attr) %>% 
#'     as_tibble() %>% 
#'     select(attribute, ei_index) %>% 
#'     rename(var = attribute)
#'   
#'   init_matrix <- rep_as_adj_matrix(x, vrt_attr = vrt_attr, leave_raw = TRUE)
#'   
#'   attrs <- sort(unique(`dim<-`(el, NULL)))
#'   inits <- replicate(length(attrs), vector("double", times))
#'   colnames(inits) <- attrs
#'   
#'   permuter <- build_permuter(init_matrix, labels = attrs)
#'   
#'   res <- do.call(rbind, replicate(1000, permuter(), simplify = FALSE))
#'   library(tidyverse)
#'   res %>% 
#'     as_tibble() %>% 
#'     gather(var, val) %>% 
#'     ggplot() +
#'     stat_density(aes(x = val)) +
#'     geom_vline(aes(xintercept = ei_index), color = "red",
#'                data = test_stat) +
#'     facet_wrap(~ var)
#'   hist(res[,1])
#'   hist(res[,2])
#'   # res2 <- do.call(rbind, res)
#'   for (i in seq_len(ncol(res2))) {
#'     hist(res2[[, i]])
#'   }
#'   
#'   # for (i in times) {
#'   #   permute_ma
#'   #   el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
#'   #   attrs <- sort(unique(`dim<-`(el, NULL)))
#'   #   inits <- replicate(length(attrs), vector("double", times))
#'   #   colnames(inits) <- attrs
#'   #   mix_mat <- table(.ego = factor(el[, ".ego"], levels = attrs),
#'   #                    .alter = factor(el[, ".alter"], levels = attrs), dnn = NULL)
#'   #   internal <- diag(mix_mat)
#'   #   diag(mix_mat) <- NA_integer_
#'   #   external <- rowSums(mix_mat, na.rm = TRUE)
#'   #   ei <- (external - internal) / (external + internal)
#'   # 
#'   #   
#'   #   inits[i, ] <- ei
#'   #   # inits[[names(ei[[i]])]][[i]] <- ei[[names(ei)[[i]]]]
#'   # }
#'   
#'   out <- data.frame(attribute = rownames(mix_mat),
#'                     external_ties = external,
#'                     internal_ties = internal,
#'                     ei_index = ei, 
#'                     stringsAsFactors = FALSE)
#'   # rownames(out) <- NULL
#'   # class(out) <- c("ei_index_grp", "data.frame")
#'   out
#' }
#' microbenchmark::microbenchmark(replicate(1000, permuter()))
#' 
#' build_permuter <- function(adj_matrix, labels, ...) {
#'   n <- seq_len(nrow(adj_matrix))
#'   # force(test_fun)
#' 
#'   function() {
#'     rownames(adj_matrix) <- rownames(adj_matrix)[sample(n)]
#'     colnames(adj_matrix) <- rownames(adj_matrix)
#'     el <- adj_mat_to_el(adj_matrix)
#'     mix_mat <- table(.ego = factor(el[, 1L], levels = labels),
#'                      .alter = factor(el[, 2L], levels = labels), dnn = NULL)
#'     internal <- diag(mix_mat)
#'     diag(mix_mat) <- NA_integer_
#'     external <- rowSums(mix_mat, na.rm = TRUE)
#'     (external - internal) / (external + internal)
#'   }
#' }
#' 
#' adj_mat_to_el <- function(x) {
#'   out <- cbind(c(row(x)), c(col(x)), c(x))
#'   out <- matrix(out[out[, 3L] == 1L], ncol = 3L)[, 1:2]
#'   matrix(rownames(x)[out], ncol = 2L)
#' }
#' 
#' 
#' # ei_index_global.igraph <- function(x, vrt_attr, drop_loops = FALSE) {
#' #   if (vrt_attr == ".name") {
#' #     terminate('".name" is not a valid vertex attribute.')
#' #   }
#' #   el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
#' #   if(drop_loops && net_has_loops(x)) {
#' #     el <- unique.matrix(el)
#' #   }
#' #   n_edges <- nrow(el)
#' #   external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
#' #   internal <- n_edges - external
#' #   
#' #   (external - internal) / n_edges
#' # }
#' # 
#' # ei_index_global.network <- function(x, vrt_attr, drop_loops = FALSE) {
#' #   if (vrt_attr == ".name") {
#' #     terminate('".name" is not a valid vertex attribute.')
#' #   }
#' #   el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
#' #   if(drop_loops && net_has_loops(x)) {
#' #     el <- unique.matrix(el)
#' #   }
#' #   n_edges <- nrow(el)
#' #   external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
#' #   internal <- n_edges - external
#' #   
#' #   (external - internal) / n_edges
#' # }
#' # 
#' # ei_index_global.adj_matrix <- function(x, drop_loops = FALSE) {
#' #   # if (attr(x, "vrt_attr") == ".name") {
#' #   #   terminate('".name" is not a valid vertex attribute.')
#' #   # }
#' #   el <- rep_as_edgelist(x, leave_raw = TRUE)
#' #   if(drop_loops && net_has_loops(x)) {
#' #     el <- unique.matrix(el)
#' #   }
#' #   n_edges <- nrow(el)
#' #   external <- length(el[, ".ego"][el[, ".ego"] != el[, ".alter"]])
#' #   internal <- n_edges - external
#' #   
#' #   (external - internal) / n_edges
#' # }
#' 
#' 
#' 
#' ei_index_vertex <- function(x, vrt_attr, drop_loops = FALSE) {
#'   stop("Not yet implemented.")
#'   x
#'   vrt_attr
#'   drop_loops
#' }
#' 
#' 

#' 
#' # #' @rdname ei_index
#' # #' 
#' # #' @export
#' # autoplot.ei_index_grp <- function(x) {
#' #   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#' #     stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
#' #   }
#' #   x$name <- factor(x$name, levels = rev(x$name))
#' #   out <- ggplot2::ggplot(x)
#' #   out <- out + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ei_index, 
#' #                                                   y = name, yend = name,color = ei_index), 
#' #                                      alpha = 0.5, show.legend = FALSE)
#' #   out <- out + ggplot2::geom_point(ggplot2::aes(x = 0, y = name), size = 4, 
#' #                                    color = "lightgray")
#' #   out <- out + ggplot2::geom_point(ggplot2::aes(x = ei_index, y = name, fill = ei_index), 
#' #                                    size = 4, shape = 21, show.legend = FALSE)
#' #   out <- out + ggplot2::scale_color_distiller(palette = "RdBu", direction = 1)
#' #   out <- out + ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1)
#' #   out <- out + ggplot2::labs(x = "E-I Index", y = "Vertex Name", 
#' #                              title = "E-I Index, Vertices")
#' #   out <- out + theme_sna()
#' #   out <- out + scale_x_ratio()
#' #   
#' #   out
#' # }
#' 
#' 
#' 
#' 
#' # adj_mat_tester <- igraph::graph("Zachary") %>% 
#' #   igraph::set_vertex_attr("name", value = c(letters, LETTERS)[seq_len(igraph::vcount(.))]) %>% 
#' #   igraph::as_adjacency_matrix(sparse = FALSE) %>% 
#' #   `class<-`(c("adj_matrix", "matrix"))
#'   
#' # adj_mat_tester
#' # permute_matrix(force)
#' # 
#' # adj_mat_permuter()
#' # 
#' # data(sampson, package = "ergm")
#' 
#' # rep_as_adj_matrix.igraph <- function(x) {
#'   # 
#' # }
#' # test_adj_mat <- sampson_monastery %>% 
#' #   edg_filter(time == 1) %>% 
#' #   rep_as_adj_matrix()
#' # 
#' # test_el1 <- test_adj_mat %>% 
#' #   adj_mat_to_el()
#' # test_el2 <- sampson_monastery %>% 
#' #   edg_filter(time == 1) %>% 
#' #   rep_as_edgelist(leave_raw = T)
#' # 
#' # m <- rbind(test_el1, test_el2)
#' # 
#' # all(apply(m, 2, function(x) length(unique(x)) == 1) == TRUE)
#' # 
#' # table(as.data.frame(test_el1))
#' # 
#' # microbenchmark::microbenchmark(adj_mat_to_el(x))
#' # rep_as_edgelist.adj_matrix <- function(x) {
#' #   out <- as.data.frame.table(x, responseName = "n",
#' #                              stringsAsFactors = FALSE)
#' #   out <- out[out[["n"]] > 0L, ]
#' #   out[["n"]] <- NULL
#' #   colnames(out) <- c(".ego", ".alter")
#' #   rownames(out) <- NULL
#' #   as_edge_data_frame(out)
#' # }
#' # 
#' # library(tidyverse)
#' # samplike %>% 
#' #   # as_igraph() %>% 
#' #   # igraph::as_adjacency_matrix()
#' #   network::set.vertex.attribute("vertex.names", 
#' #                                 network::get.vertex.attribute(samplike, "group")) %>% 
#' #   network::as.matrix.network() %>% 
#' #   permute_matrix() %>% 
#' #   rep_as_edgelist.adj_matrix() %>% 
#' #   network::network() %>% 
#' #   ei_index_global("group")
#' #   
#' #   
#' #   
#' #   permute_matrix() %>% 
#' #   as_tibble() %>% 
#' #   gather(var, val)
#' #   igraph::graph_from_edgelist() %>% 
#'   
