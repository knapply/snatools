theme_sna <- function(base_size = 11, base_family = "serif", legend.position = "top") {
  out <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
  out <- out + ggplot2::theme(legend.position = legend.position)
  
  out
}