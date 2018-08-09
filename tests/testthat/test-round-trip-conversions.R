context("Round trip conversion: undirected graphs")

ig_undir <- build_test_graph("ig", direct = FALSE) %>% 
  sna_clean_graph()
nw_undir <- build_test_graph("nw", direct = FALSE) %>% 
  sna_clean_graph()

test_that("undirected igraph makes round trip unchanged", {
  expect_equal(unclass(sna_as_igraph(sna_as_network(ig_undir)))[1:9],
               unclass(ig_undir)[1:9])
})

test_that("undirected network makes round trip unchanged", {
  expect_equal(sna_as_network(sna_as_igraph(nw_undir)), nw_undir)
})

context("Round trip conversion: directed graphs")

ig_dir <- build_test_graph("ig") %>% 
  sna_clean_graph()
nw_dir <- build_test_graph("nw") %>% 
  sna_clean_graph()

test_that("undirected igraph makes round trip unchanged", {
  expect_equal(unclass(sna_as_igraph(sna_as_network(ig_dir)))[1:9],
               unclass(ig_dir)[1:9])
})

test_that("undirected network makes round trip unchanged", {
  expect_equal(sna_as_network(sna_as_igraph(nw_dir)), nw_dir)
})

context("Round trip conversion: bipartite graphs")

ig_bipart <- build_test_graph("ig", bipart = TRUE) %>% 
  sna_clean_graph()

target_url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/2mode/divorce.net"
nw_bipart <- snatools::divorce_nw %>% 
  sna_clean_graph()

test_that("undirected igraph makes round trip unchanged", {
  expect_equal(unclass(sna_as_igraph(sna_as_network(ig_bipart)))[1:9],
               unclass(ig_bipart)[1:9])
})

test_that("undirected network makes round trip unchanged", {
  expect_equal(sna_as_network(sna_as_igraph(nw_bipart)), nw_bipart)
})


