#' @export
build_test_graph <- function(ig_or_nw, direct = TRUE, bipart = FALSE,
                             n = 30, p = 0.25, ...) {
  if(ig_or_nw == "ig"){
    if(!bipart){
      graph <- igraph::erdos.renyi.game(n, p, directed = direct, ...)
    } else {
      graph <- igraph::bipartite.random.game(n, n, p = p, directed = FALSE, ...)
    }
    graph <- graph %>% 
      igraph::set_vertex_attr("name", value = paste0("vrt",
                                           seq_len(igraph::vcount(.)))) %>% 
      igraph::set_vertex_attr("node_character", 
                              value = sample(letters, igraph::vcount(.), 
                                             replace = TRUE)) %>% 
      igraph::set_vertex_attr("node_integer", 
                              value = sample(seq_len(1000), igraph::vcount(.),
                                             replace = TRUE)) %>% 
      igraph::set_vertex_attr("node_double", 
                              value = runif(igraph::vcount(.), 0, 1000)) %>% 
      igraph::set_edge_attr("edge_character", 
                            value = sample(LETTERS, igraph::ecount(.), 
                                           replace = TRUE)) %>% 
      igraph::set_edge_attr("edge_integer", 
                            value = sample(seq_len(1000), igraph::ecount(.), 
                                           replace = TRUE)) %>% 
      igraph::set_edge_attr("edge_double", value = runif(igraph::ecount(.), 0, 1000))
  }
  if(ig_or_nw == "nw"){
    if(!bipart){
      graph <- rbinom(n^2, 1, p) %>% 
        matrix(n, n) %>% 
        network::network(directed = direct)
    } else {
      inc_mat <- agilenet::southern_women %>% 
        standardize_bipartite(type) %>% 
        igraph::as_incidence_matrix()
      graph <- network::network.initialize(n = vcount(agilenet::southern_women ),
                                           directed = FALSE, 
                                           bipartite = nrow(inc_mat))
        
    }
    graph <- graph %>% 
      network::set.vertex.attribute("vertex.names", 
                                    paste0("node_", seq_len(network::network.size(.)))) %>% 
      network::set.vertex.attribute("node_character", 
                                    value = sample(letters, network::network.size(.),
                                                   replace = TRUE)) %>% 
      network::set.vertex.attribute("node_integer", 
                                    value = sample(seq_len(1000), network::network.size(.),
                                                   replace = TRUE)) %>% 
      network::set.vertex.attribute("node_double", 
                                    value = runif(network::network.size(.), 0, 1000)) %>% 
      network::set.edge.attribute("edge_character", 
                                  value = sample(LETTERS, network::network.edgecount(.),
                                                 replace = TRUE)) %>% 
      network::set.edge.attribute("edge_integer",
                                  value = sample(seq_len(1000),
                                                 network::network.edgecount(.),
                                                 replace = TRUE)) %>% 
      network::set.edge.attribute("edge_double", 
                                    value = runif(network::network.edgecount(.), 0, 1000))
  }
  graph
}
