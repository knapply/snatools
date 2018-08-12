context("Round trip conversion: undirected graphs")

ig_undir <- build_test_graph("ig", direct = FALSE) %>% 
  clean_graph()
nw_undir <- build_test_graph("nw", direct = FALSE) %>% 
  clean_graph()

test_that("undirected igraph makes round trip unchanged", {
  expect_true(ig_undir %==% as_igraph(as_network(ig_undir)))
})

test_that("undirected network makes round trip unchanged", {
  expect_true(nw_undir %==% as_network(as_igraph(nw_undir)))
})

context("Round trip conversion: directed graphs")

ig_dir <- build_test_graph("ig") %>% 
  clean_graph()
nw_dir <- build_test_graph("nw") %>% 
  clean_graph()

test_that("undirected igraph makes round trip unchanged", {
  expect_true(ig_dir %==% as_igraph(as_network(ig_dir)))
})

test_that("undirected network makes round trip unchanged", {
  expect_true(nw_dir %==% as_network(as_igraph(nw_dir)))
})

context("Round trip conversion: bipartite graphs")

ig_bipart <- build_test_graph("ig", bipart = TRUE) %>% 
  clean_graph()

target_url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/2mode/divorce.net"
nw_bipart <- snatools::divorce_nw %>% 
  clean_graph()

test_that("undirected igraph makes round trip unchanged", {
  expect_true(ig_bipart %==% as_igraph(as_network(ig_bipart)))
})

test_that("undirected network makes round trip unchanged", {
  expect_true(nw_bipart %==% as_network(as_igraph(nw_bipart)))
})


