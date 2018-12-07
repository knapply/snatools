#' Quickly plot a graph's triad census.
#' 
#' @seealso [igraph::triad_census()], [sna::triad.census()]
#' 
#' @examples
#' library(snatools)
#' 
#' data("UKfaculty", package = "igraphdata")
#' 
#' UKfaculty %>% 
#'   plot_triad_census(coord_flip = FALSE)
#' 
#' @export
#' 
plot_triad_census <- function(x, coord_flip = TRUE, pal = rainbow) {
  UseMethod("plot_triad_census")
}

#' @rdname plot_triad_census
#' 
#' @export
#' 
plot_triad_census.igraph <- function(x, coord_flip = TRUE, pal = rainbow) {
  if(!require(ggplot2, quietly = TRUE)) {
    stop('`plot_triad_census()` requires {ggplot2}... install.packages("ggplot2")')
  }
  tc_df <- snatools::triads_df
  tc_df$ycoord <- seq_len(nrow(tc_df)) - 0.5
  
  suppressWarnings(tc_df$count <- igraph::triad_census(x))
  
  if(!igraph::is_directed(x)) {
    tc_df <- tc_df[tc_df$state %in% c("003", "102", "300"), ]
      triad_grobs <- snatools:::triad_grobs$undirected
  } else {
    triad_grobs <- snatools:::triad_grobs$directed
  }
  
  if(coord_flip){
    tc_df$state <- factor(tc_df$state, levels = rev(tc_df$state))
    if(is.function(pal)) {
      tc_df$fill <- rev(pal(nrow(tc_df)))
    } else {
      tc_df$fill <- pal
    }
  } else {
    triad_grobs <- rev(triad_grobs)
    tc_df$state <- factor(tc_df$state, levels = tc_df$state)
        if(is.function(pal)) {
      tc_df$fill <- pal(nrow(tc_df))
    } else {
      tc_df$fill <- pal
    }
  }
    
  tc_gg <- ggplot2::ggplot(tc_df) +
    ggplot2::geom_col(aes(state, count, fill = fill), show.legend = FALSE) +
    ggplot2::scale_y_continuous(limits = c(0 - (max(tc_df$count) * 0.05), max(tc_df$count))) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(y = NULL, x = NULL) +
    ggplot2::theme_minimal(10, "serif") +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line()
                   # axis.text.y = ggplot2::element_blank()
                   )
  
  start <- nrow(tc_df) + 0.5
  for(i in triad_grobs) {
    tc_gg <- tc_gg +
      ggplot2::annotation_custom(i, xmin = start, xmax = start - 1,
                                 ymin = -Inf,
                                 ymax = 0) +
      ggplot2::annotate("segment", x = start, xend = start, y = -Inf, yend = Inf,
                        color = "lightgray", size = 1.05)
    start <- start - 1
  }
  tc_gg <- tc_gg +
    ggplot2::annotate("segment", x = 0.5, xend = 0.5, y = -Inf, yend = Inf, 
                      color = "gray")
  if(!coord_flip) {
    return(tc_gg)
  }
  tc_gg + ggplot2::coord_flip()
}

# graph("Zachary") %>% 
#   plot_triad_counts()
# 
# snatools:::build_test_graph("ig", n = 30, p = 0.35) %>% 
#   plot_triad_counts()
# ggsave("tests/triad-census.png", width = 10, height = 14, dpi = 600)






