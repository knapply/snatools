adj_mat <- get_test_mat(.directed = F)

el <- get_test_el(.directed = F)

nw <- network::network.initialize(
  n = nrow(adj_mat),  directed = F, loops = FALSE,
  hyper = FALSE, multiple = FALSE, bipartite = FALSE
)

nw

network::network.edgelist(x = el, g = nw)

nw_el <- network::as.matrix.network.edgelist(nw)
attr(nw_el, "n") <- NULL
attr(nw_el, "vnames") <- NULL

all.equal(el, nw_el)

ig_el <- igraph::graph_from_edgelist(el, directed = FALSE) %>% 
  igraph::as_edgelist()

all.equal(nw_el, get_test_el(.directed = F))


all.equal(nw_el, get_test_el_ig(.directed = F))
