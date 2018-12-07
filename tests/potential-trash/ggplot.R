# #' @importFrom ggplot2 autoplot
# #' @export
# ggplot2::autoplot

# check_ggplot <- function() {
#   if(!requireNamespace("ggplot2", quietly = TRUE)) {
#     stop('The `ggplot2` package is required for this functionality. 
#           Install it via `install.packages("ggplot2")`', call. = FALSE)
#   }
# }

theme_sna <- function(base_size = 11, base_family = "serif", legend.position = "top") {
  out <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
  out <- out + ggplot2::theme(legend.position = legend.position)
  
  out
}

scale_x_ratio <- function(limits = c(-1, 1)) {
  ggplot2::scale_x_continuous(limits = limits)
}

scale_y_ratio <- function(limits = c(-1, 1)) {
  check_ggplot()
  
  ggplot2::scale_y_continuous(limits = limits)
}