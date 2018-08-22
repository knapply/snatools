scrambled <- southern_women_ig %>% 
  permute.vertices(sample(V(.))) %>% 
  clean_graph.igraph()

southern_women_ig %>% vrt_attrs()

scrambled %>% vrt_attrs()

scrambled %>% as_network() %>% vrt_attrs()

as_igraph(as_network(scrambled)) %>% vrt_attrs()

all.equal(scrambled, as_igraph(as_network(scrambled)))

all.equal(
unclass(scrambled)[9][[1]][[3]]$type
,
unclass(as_igraph(as_network(scrambled)))[9][[1]][[3]]$type
)

new_order <- rev(order(V(scrambled)$type))

true_nodes <- which(V(scrambled)$type)
false_nodes <- which(!V(scrambled)$type)

c(true_nodes, false_nodes)
vrt_names <- V(scrambled)$name
vrt_attrs(scrambled)
vrt_names[new_order]

sw_matrix <- matrix(
  c(1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
    1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 
    1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
    0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
    1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,
    1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 
    0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0),
  nrow = 14L, ncol = 18L,
  dimnames = list(
    c("E1", "E2", "E3", "E4", "E5", "E6", "E7",
      "E8", "E9", "E10", "E11", "E12", "E13", "E14"),
    c("EVELYN", "LAURA", "THERESA", "BRENDA", "CHARLOTTE", "FRANCES",
      "ELEANOR", "PEARL", "RUTH", "VERNE", "MYRNA", "KATHERINE", 
      "SYLVIA", "NORA", "HELEN", "DOROTHY", "OLIVIA", "FLORA")))

sw_matrix %>% 
  as.network.matrix(bipartite = 18) %>% 
  clean_graph() %>% 
  permute.vertexIDs(seq_along(sample(seq_len(network.size(.))))) %>% 
  # permute.vertexIDs(seq_along(sample(seq_len(network.size(.))))) %>% 
  print()
  # as.network(bipartite = 18)

  