library(igraph)
library(tidyverse)
library(ggraph)

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

undirected_triads <- list(
  triad_003 = make_graph(~ A, B, C),
  triad_102 = graph(c("A", "B", "B", "A"), directed = FALSE) %>% 
    add_vertices(nv = 1, attr = list(name = "C")),
  triad_300 = graph(c("A", "B", "B", "A", "B", "C", "C", "B", "A", "C", "C", "A"),
                    directed = FALSE)
)


plotter <- function(x) {
  require(ggraph)
  coords <- matrix(c(0, 0.9,
                     -0.4, 0.2,
                     0.4, 0.2), ncol = 2, byrow = TRUE)
  # shape = str_remove(label, "^triad_")
  out <- ggraph(x, layout = coords)
  if(igraph::is_directed(x))
    out <- out +
    geom_edge_link(linejoin = "mitre",
                    arrow = arrow(angle = 40, length = unit(1.15, "mm"), type = "closed"),
                    start_cap = circle(1.15, "mm"),
                    end_cap = circle(1.15, "mm")) 
    # geom_text(aes(0, 0.415, label = shape), size = 3)
  if(!igraph::is_directed(x)) {
    out <- out +
      geom_edge_link() 
      # geom_text(aes(0, 0.4, label = shape), size = 6)
  }
  
  out + 
    scale_x_continuous(limits = c(-0.5, 0.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    geom_node_point(color = "red") +
    theme_void() +
    theme(plot.background = element_rect("transparent"),
          panel.background = element_rect("transparent"))
}

triad_grobs_directed <- directed_triads %>% 
  map(plotter) %>% 
  map(`class<-`, c("gg", "ggplot")) %>% 
  map(ggplotGrob)

triad_grobs_undirected <- undirected_triads %>% 
  map(plotter) %>% 
  map(`class<-`, c("gg", "ggplot")) %>% 
  map(ggplotGrob)

triad_grobs <- list(directed = triad_grobs_directed, 
                    undirected = triad_grobs_undirected)

# devtools::use_data(triad_grobs,
                   # internal = TRUE, overwrite = TRUE)
