context("Round trip conversion: simple graph")

zachary <- igraph::graph("Zachary")

test_that("simple igraph makes round trip unchanged", {
  expect_true(zachary %==% as_igraph(as_network(zachary)))
})

context("Round trip conversion: undirected graphs")

ig_undir <- snatools:::build_test_graph("ig", direct = FALSE)
nw_undir <- snatools:::build_test_graph("nw", direct = FALSE)

test_that("undirected igraph makes round trip unchanged", {
  expect_true(ig_undir %==% as_igraph(as_network(ig_undir)))
})

as_igraph(as_network(ig_undir)) %>% snatools:::edg_get_attrs.igraph()
ig_undir %>% snatools:::edg_get_attrs.igraph()

test_that("undirected network makes round trip unchanged", {
  expect_true(nw_undir %==% as_network(as_igraph(nw_undir)))
})

context("Round trip conversion: directed graphs")

ig_dir <- snatools:::build_test_graph("ig")
nw_dir <- snatools:::build_test_graph("nw")

test_that("undirected igraph makes round trip unchanged", {
  expect_true(ig_dir %==% as_igraph(as_network(ig_dir)))
})

test_that("undirected network makes round trip unchanged", {
  expect_true(nw_dir %==% as_network(as_igraph(nw_dir)))
})

context("Round trip conversion: bipartite graphs")

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

ig_bipart <- igraph::graph_from_incidence_matrix(sw_matrix) %>% 
  igraph::permute.vertices(sample(igraph::V(.))) 

nw_bipart <- network::as.network.matrix(t(sw_matrix), bipartite = TRUE) #%>% 
  # bip_clarify_actors()

nw_bipart_swap <- network::as.network.matrix(sw_matrix, bipartite = TRUE) %>% 
  bip_swap_modes()

random_attr <- sample(seq_len(nw_bipart$gal$n))

nw_bipart_random_attrs <- network::as.network.matrix(t(sw_matrix), bipartite = TRUE) %>% 
  network::set.vertex.attribute("test_attr", random_attr) %>% 
  bip_clarify_actors()

random_attr_swap <- c(random_attr[(nw_bipart_random_attrs$gal$bipartite + 1):nw_bipart_random_attrs$gal$n],
                      random_attr[1:nw_bipart_random_attrs$gal$bipartite])

nw_bipart_swap_random_attrs <- network::as.network.matrix(sw_matrix, bipartite = TRUE) %>% 
  network::set.vertex.attribute("test_attr", random_attr_swap) %>% 
  bip_swap_modes()


 
test_that("bipartite igraph makes round trip unchanged", {
  expect_true(ig_bipart %==% as_igraph(as_network(ig_bipart)))
})

test_that("bipartite network (w/ is_actor attr) makes round trip unchanged", {
  expect_true(nw_bipart %==% as_network(as_igraph(nw_bipart)))
})

test_that("bipartite network + bip_swap_modes()", {
  expect_true(nw_bipart_random_attrs %==% nw_bipart_swap_random_attrs)
})
