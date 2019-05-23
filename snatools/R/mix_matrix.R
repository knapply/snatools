#' Construct a graph object's mixing matrix.
#' 
#' Using a categorical vertex attribute, construct a matrix depicting network mixing
#' patterns.
#' 
#' @template graph-param
#' @template vrt_attr-param
#' 
#' @template bknapp-author
#' 
#' @examples 
#' library(snatools)
#' 
#' data(samplk, package = "ergm")
#' 
#' samplk1 %>% 
#'   rep_as_mixing_matrix(vrt_attr = "group")
#' 
#' @export
rep_as_mixing_matrix <- function(x, vrt_attr) {
  attrs <- vrt_get_attr(x, vrt_attr)
  el <- matrix(attrs[get_el(x)], ncol = 2L)
  dim_names <- sort(unique(attrs))
  out <- table("Ties Sent" = factor(el[, 1], levels = dim_names),
               "Ties Received" = factor(el[, 2], levels = dim_names))
  `class<-`(out, "matrix")
}
