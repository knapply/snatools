library(igraph)
library(tidyverse)
library(ggraph)
library(hexSticker)
library(magick)

directed_triads <- list(
  triad_003 = make_graph(~ A, B, C) %>% 
    igraph::as.directed(),
  triad_012 =  igraph::make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("B", "A")),
  triad_102 = graph(c("A", "B", "B", "A")) %>% add_vertices(nv = 1, attr = list(name = "C")),
  triad_021D = graph(c("B", "A", "B", "C")),
  triad_021U = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("B", "A", "C", "A")),
  triad_021C = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("B", "A", "A", "C")),
  triad_111D = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("A", "C", "B", "C", "C", "B")),
  triad_111U = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("C", "A", "B", "C", "C", "B")),
  triad_030T = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("C", "A", "B", "A", "B", "C")),
  triad_030C = graph(c("B", "A", "C", "B", "A", "C")),
  triad_201 = graph(c("A", "B", "B", "A", "B", "C", "C", "B")),
  triad_120D = graph(c("B", "A", "B", "C", "A", "C", "C", "A")),
  triad_120U = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("B", "A", "C", "A", "B", "C", "C", "B")),
  triad_120C = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("B", "A", "A", "C", "B", "C", "C", "B")),
  triad_210 = make_empty_graph(3) %>% 
    set_vertex_attr("name", value = c("A", "B", "C")) %>% 
    add_edges(c("B", "A", "B", "C", "C", "B", "A", "C", "C", "A")),
  triad_300 = graph(c("A", "B", "B", "A", "B", "C", "C", "B", "A", "C", "C", "A"))
)

plotter <- function(x, lab) {
  coords <- matrix(c(0, 0.9,
                     -0.4, 0.2,
                     0.4, 0.2), ncol = 2, byrow = TRUE)
  shape <- str_remove(lab, "^triad_")
  out <- ggraph(x, layout = coords)
  if(igraph::is_directed(x)) {
    out <- out +
      geom_edge_link(arrow = arrow(angle = 35, length = unit(6, "mm"), type = "closed"),
                     start_cap = circle(5, "mm"),
                     end_cap = circle(5, "mm"), color = "white") +
      NULL
  }
  if(!igraph::is_directed(x)) {
    out <- out +
      geom_edge_link()
  }
  out + 
    scale_x_continuous(limits = c(-0.5, 0.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    geom_node_point(color = "white", size = 8) +
    geom_node_point(color = "#1d479b", size = 6) +
    labs(x = NULL, y = NULL) +
    theme_void() +
    theme_transparent()
}

# plot ====
triads_gg <- directed_triads %>% 
  imap(plotter)

triads_gg <- cowplot::plot_grid(plotlist = triads_gg)


if(!dir.exists("man/figures")) {
  dir.create("man/figures/")
}

le_width = 8
le_height = 8

ggsave(filename = "man/figures/triads_gg.png", plot = triads_gg,
       width = le_width, height = le_height, dpi = 800, bg = "transparent")


set.seed(1234) 
adj_mat_gg <- igraph::random.graph.game(30, 0.2) %>% 
  as_adjacency_matrix(sparse = FALSE) %>% 
  as_tibble() %>% 
  mutate(row = row_number()) %>% 
  gather(col, val, -row) %>%
  mutate(col = as.numeric(str_extract(col, "\\d+"))) %>% 
  ggplot(aes(col, row)) +
  geom_text(aes(label = val, family = "mono"), color = "#545454",
            size = 30,
            show.legend = FALSE) +
  scale_x_continuous(limits = c(0, 31)) +
  scale_y_continuous(limits = c(0, 31)) +
  theme_void() +
  coord_equal() +
  labs(x = NULL, y = NULL)

logo_gg <- adj_mat_gg +
  annotation_custom(ggplotGrob(triads_gg))

ggsave(filename = "man/figures/logo_gg.png", plot = logo_gg,
       width = 8, height = 8, dpi = 600, bg = "transparent")

img <- image_read("man/figures/logo_gg.png")

# hex sticker ====
sticker(subplot = img,
  s_x = 1,
  s_y = 1,
  s_width = 1.275,
  s_height = 1.275,
  package = "snatools",
  p_y = 1.735,
  p_size = 55,
  p_color = "white",
  h_color = "red",
  h_fill = "black",
  u_size = 12,
  u_color = "white",
  # spotlight = TRUE,  #l_width = 5,
  # l_height = 1,
  # l_alpha = 0.5, 
  # l_y = 0.25, 
  url = "knapply.github.io/snatools",
  filename = "man/figures/logo.png")

ggsave(width = 43.9 * 1.5, height = 50.8 * 1.5, dpi = 800,
       filename = "man/figures/logo.png",
       bg = "transparent", units = "mm")
