#' Visualization
#'
#' @export
#' 
#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' 
# autoplot <- function(x, ...) {
#   UseMethod("autoplot")
# }

#' @export
#' 
theme_sna <- function(base_size = 11, base_family = "serif", legend.position = "top") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  out <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
  out <- out + ggplot2::theme(legend.position = legend.position)
  
  out
}

#' @export
#' 
scale_x_ratio <- function(limits = c(-1, 1)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  ggplot2::scale_x_continuous(limits = limits)
}

#' @export
#' 
scale_y_ratio <- function(limits = c(-1, 1)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop('The {ggplot2} package is required for this functionality.', call. = FALSE)
  }
  ggplot2::scale_y_continuous(limits = limits)
}