#' E-I Index
#' 
#' Given a categorical vertex attribute describing mutually exclusive groups, the E-I 
#' index represents a ratio of external to internal ties.
#' 
#' @template graph-param
#' @template vrt_attr-param
#' @param scope `character` indicating the target scope. Choices: `"global"`, `"group"`, 
#' or `"vertex"`.
#' @param xlim `numeric` `vector` specifying lower and upper limits of 
#' `autoplot.ei_index`'s x-axis.
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' library(ggplot2)
#' 
#' data(samplk, package = "ergm")
#' 
#' samplk1 %>% 
#'   ei_index(vrt_attr = "group", scope = "global")
#'
#' samplk1 %>% 
#'   ei_index(vrt_attr = "group", scope = "group") %>% 
#'   autoplot()
#'   
#' samplk1 %>% 
#'   ei_index(vrt_attr = "group", scope = "vertex") %>% 
#'   autoplot()
#' 
#' @export
ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex")) {
  validate_args(x, vrt_attr, validate_graph = TRUE)
  scope <- match.arg(scope, c("global", "group", "vertex"))
  switch(scope,
         global = ei_global(x, vrt_attr),
         group = ei_group(x, vrt_attr),
         vertex = ei_vertex(x, vrt_attr))
}

ei_global <- function(x, vrt_attr) {
  attrs <- vrt_get_attr(x, vrt_attr)
  el <- matrix(attrs[get_el(x)], ncol = 2L)
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
  out <- tibble(attribute = rownames(mix_mat),
                external_ties = external,
                internal_ties = internal,
                ei_index = ei)
  
  `class<-`(out, c("ei_index", "tibble", "data.frame"))
}

#' @importFrom tibble as_tibble tibble
ei_vertex <- function(x, vrt_attr) {
  vrt_names <- vrt_get_names(x)
  adj_list <- rep_as_adj_list(x, vrt_attr = vrt_attr)
  init_df <- mapply(function(ego, alters) {
    n_edges <- length(alters)
    external <- length(alters[alters != ego])
    internal <- n_edges - external
    ei <- (external - internal) / n_edges
    data.frame(attribute = ego,
               external_ties = external,
               internal_ties = internal,
               ei_index = ei,
               stringsAsFactors = FALSE)
    }, names(adj_list), adj_list, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  out <- cbind.data.frame(data.frame(vertex = vrt_names, 
                                     stringsAsFactors = FALSE),
                          do.call(rbind.data.frame, 
                                  c(init_df, stringsAsFactors = FALSE)),
                          stringsAsFactors = FALSE)
  `class<-`(as_tibble(out), c("ei_index", "tibble", "data.frame"))
}

#' @rdname ei_index
#' 
#' @importFrom glue glue
#' @importFrom rlang !! sym
#' @export
autoplot.ei_index <- function(x, xlim = c(-1, 1)) {
  if ("vertex" %in% colnames(x)) {
    y_axis <- sym("vertex")
    y_lab <- "Vertex Names"
    title_lab <- "Vertices"
  } else {
    y_axis <- sym("attribute")
    y_lab <- "Attribute Category"
    title_lab <- "Groups"
  }
  out <- ggplot2::ggplot(data = x)
  out <- out + ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ei_index, 
                                                  y = !!y_axis,
                                                  yend = !!y_axis, 
                                                  color = ei_index), 
                                     alpha = 0.5, show.legend = FALSE)
  out <- out + ggplot2::geom_point(ggplot2::aes(x = 0, y = !!y_axis),
                                   size = 4, color = "lightgray")
  out <- out + ggplot2::geom_point(ggplot2::aes(x = ei_index, 
                                                y = !!y_axis, 
                                                fill = ei_index), 
                                   size = 4, shape = 21, color = "lightgray", 
                                   show.legend = FALSE)
  out <- out + ggplot2::scale_color_distiller(palette = "RdBu", direction = 1)
  out <- out + ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1)
  out <- out + ggplot2::labs(x = "E-I Index", y = y_lab,
                             title = glue("E-I Index, {title_lab}"))
  out <- out + theme_sna()
  if (!is.null(xlim)) {
    out <- out + ggplot2::scale_x_continuous(limits = xlim)
  }
  out
}

#' @importFrom tibble as_tibble
print.ei_index <- function(x, ...) {
  print(tibble::as_tibble(x), ...)
}


