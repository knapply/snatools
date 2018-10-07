# 
# 
# library(igraph)
# 
# triad_003 <- make_graph(~ A, B, C)
# 
# triad_012 <- graph(c("B", "A")) %>% add_vertices(nv = 1, attr = list(name = "C"))
# 
# triad_102 <- graph(c("A", "B", "B", "A")) %>% add_vertices(nv = 1, attr = list(name = "C"))
# 
# triad_021D <- graph(c("B", "A", "B", "C"))
# 
# triad_021U <- graph(c("A", "B", "C", "B"))
# 
# triad_021C <- graph(c("A", "B", "B", "C"))
# 
# triad_111D <- graph(c("A", "B", "B", "A", "C", "B"))
# 
# triad_111U <- graph(c("A", "B", "B", "A", "B", "C"))
# 
# triad_030T <- graph(c("A", "B", "C", "B", "A", "C"))
# 
# triad_030C <- graph(c("B", "A", "C", "B", "A", "C"))
# 
# triad_202 <- graph(c("A", "B", "B", "A", "B", "C", "C", "B"))
# 
# triad_120D <- graph(c("B", "A", "B", "C", "A", "C", "C", "A"))
# 
# triad_120U <- graph(c("A", "B", "C", "B", "A", "C", "C", "A"))
# 
# triad_120C <- graph(c("A", "B", "B", "C", "A", "C", "C", "A"))
# 
# triad_210 <- graph(c("A", "B", "B", "C", "C", "B", "A", "C", "C", "A"))
# 
# triad_300 <- graph(c("A", "B", "B", "A", "B", "C", "C", "B", "A", "C", "C", "A"))
# 
# all_triads_ig <- list(
#   triad_003 = make_graph(~ A, B, C),
#   triad_012 =  igraph::make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("B", "A")),
#   triad_102 = graph(c("A", "B", "B", "A")) %>% add_vertices(nv = 1, attr = list(name = "C")),
#   triad_021D = graph(c("B", "A", "B", "C")),
#   triad_021U = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("B", "A", "C", "A")),
#   triad_021C = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("B", "A", "A", "C")),
#   triad_111D = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("A", "C", "B", "C", "C", "B")),
#   triad_111U = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("C", "A", "B", "C", "C", "B")),
#   triad_030T = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("C", "A", "B", "A", "B", "C")),
#   triad_030C = graph(c("B", "A", "C", "B", "A", "C")),
#   triad_201 = graph(c("A", "B", "B", "A", "B", "C", "C", "B")),
#   triad_120D = graph(c("B", "A", "B", "C", "A", "C", "C", "A")),
#   triad_120U = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("B", "A", "C", "A", "B", "C", "C", "B")),
#   triad_120C = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("B", "A", "A", "C", "B", "C", "C", "B")),
#   triad_210 = make_empty_graph(3) %>% 
#     set_vertex_attr("name", value = c("A", "B", "C")) %>% 
#     add_edges(c("B", "A", "B", "C", "C", "B", "A", "C", "C", "A")),
#   triad_300 = graph(c("A", "B", "B", "A", "B", "C", "C", "B", "A", "C", "C", "A"))
# )
# 
# 
# plotter <- function(x) {
#   require(ggraph)
#   coords <- matrix(c(0, 0.9,
#                      -0.9, 0.2,
#                      0.9, 0.2), ncol = 2, byrow = TRUE)
#   ggraph(x, layout = coords) +
#     geom_edge_fan(arrow = arrow(length = unit(1, "mm"), type = "closed"),
#                   start_cap = circle(2, 'mm'),
#                   end_cap = circle(2, 'mm')
#                   ) +
#     scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
#     scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
#     geom_node_point(color = "red") +
#     theme_void() +
#     theme(plot.background = element_rect("transparent"),
#           panel.background = element_rect("transparent"))
# }
# 
# all_triads_ig %>% 
#   map(plotter) %>% 
#   # wrap_plots()
#   gridExtra::arrangeGrob(grobs = ., padding = unit(0, "line"), ncol = 1) %>%
#   gridExtra::grid.arrange()