context("Round trip conversion: simple graph")

zachary <- igraph::graph("Zachary") %>% 
  clean_graph()

test_that("simple igraph makes round trip unchanged", {
  expect_true(zachary %==% as_igraph(as_network(zachary)))
})

context("Round trip conversion: undirected graphs")

ig_undir <- snatools:::build_test_graph("ig", direct = FALSE) %>% 
  clean_graph()
nw_undir <- snatools:::build_test_graph("nw", direct = FALSE) %>% 
  clean_graph()

test_that("undirected igraph makes round trip unchanged", {
  expect_true(ig_undir %==% as_igraph(as_network(ig_undir)))
})

test_that("undirected network makes round trip unchanged", {
  expect_true(nw_undir %==% as_network(as_igraph(nw_undir)))
})

context("Round trip conversion: directed graphs")

ig_dir <- snatools:::build_test_graph("ig") %>% 
  clean_graph()
nw_dir <- snatools:::build_test_graph("nw") %>% 
  clean_graph()

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
  igraph::permute.vertices(sample(igraph::V(.))) %>% 
  clean_graph()

nw_bipart <- network::as.network.matrix(t(sw_matrix), bipartite = TRUE) %>% 
  clean_graph()
 
test_that("bipartite igraph makes round trip unchanged", {
  expect_true(ig_bipart %==% as_igraph(as_network(ig_bipart)))
})

test_that("bipartite network makes round trip unchanged with corrected `actor_type`", {
  expect_true(nw_bipart %==% as_network(as_igraph(nw_bipart)))
})

