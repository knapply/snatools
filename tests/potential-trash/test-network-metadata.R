ig_dir <- function() snatools:::build_test_graph("ig", isolate = TRUE)
ig_undir <- function() snatools:::build_test_graph("ig", directed = FALSE, isolate = TRUE)
ig_bip <- function() snatools:::build_test_graph("ig", directed = FALSE, bipartite = TRUE)

nw_dir <- function() snatools:::build_test_graph("nw", isolate = TRUE)
nw_undir <- function() snatools:::build_test_graph("nw", directed = FALSE, isolate = TRUE)
nw_bip <- function() snatools:::build_test_graph("nw", directed = FALSE, bipartite = TRUE)

bridge_net_dir <- function() as_bridge_net(ig_dir())
bridge_net_undir <- function() as_bridge_net(ig_undir())
bridge_net_bip <- function() as_bridge_net(ig_bip())



# net_is_directed ====
context("net_is_directed")
#* directed ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_is_directed(ig_dir()), net_is_directed(nw_dir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_is_directed(ig_dir()), net_is_directed(bridge_net_dir()))
  })
#* undirected ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_is_directed(ig_undir()), net_is_directed(nw_undir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_is_directed(ig_undir()), net_is_directed(bridge_net_undir()))
  })
#* bipartite ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_is_directed(ig_bip()), net_is_directed(nw_bip()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_is_directed(ig_bip()), net_is_directed(bridge_net_bip()))
  })

# net_is_bipartite ====
context("net_is_bipartite")
#* directed ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_is_bipartite(ig_dir()), net_is_bipartite(nw_dir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_is_bipartite(ig_dir()), net_is_bipartite(bridge_net_dir()))
  })
#* undirected ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_is_bipartite(ig_undir()), net_is_bipartite(nw_undir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_is_bipartite(ig_undir()), net_is_bipartite(bridge_net_undir()))
  })
#* bipartite ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_is_bipartite(ig_bip()), net_is_bipartite(nw_bip()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_is_bipartite(ig_bip()), net_is_bipartite(bridge_net_bip()))
  })


# edge counts ====
context("edge counts")
#* directed ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_count_edges(ig_dir()), net_count_edges(nw_dir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_count_edges(ig_dir()), net_count_edges(bridge_net_dir()))
  })
#* undirected ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_count_edges(ig_undir()), net_count_edges(nw_undir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_count_edges(ig_undir()), net_count_edges(bridge_net_undir()))
  })
#* bipartite ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_count_edges(ig_bip()), net_count_edges(nw_bip()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_count_edges(ig_bip()), net_count_edges(bridge_net_bip()))
  })


# vertex counts ====
context("vertex counts")
#* directed ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_count_vertices(ig_dir()), net_count_vertices(nw_dir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_count_vertices(ig_dir()), net_count_vertices(bridge_net_dir()))
  })
#* undirected ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_count_vertices(ig_undir()), net_count_vertices(nw_undir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_count_vertices(ig_undir()), net_count_vertices(bridge_net_undir()))
  })
#* bipartite ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_count_vertices(ig_bip()), net_count_vertices(nw_bip()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_count_vertices(ig_bip()), net_count_vertices(bridge_net_bip()))
  })

# loops ====
context("net_has_loops")
#* directed ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_has_loops(ig_dir()), net_has_loops(nw_dir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_has_loops(ig_dir()), net_has_loops(bridge_net_dir()))
  })
#* undirected ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_has_loops(ig_undir()), net_has_loops(nw_undir()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_has_loops(ig_undir()), net_has_loops(bridge_net_undir()))
  })
#* bipartite ====
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_has_loops(ig_bip()), net_has_loops(nw_bip()))
  })
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_has_loops(ig_bip()), net_has_loops(bridge_net_bip()))
  })


# isolates ====
#* directed ====
context("net_has_isolates ig_dir() vs nw_dir()")
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_has_isolates(ig_dir()), net_has_isolates(nw_dir()))
  })
context("net_has_isolates ig_dir() vs bridge_net_dir()")
test_that("ig_dir() vs bridge_net_dir())", {
  expect_equal(net_has_isolates(ig_dir()), net_has_isolates(bridge_net_dir()))
  })
#* undirected ====
context("net_has_isolated ig_undir() vs nw_undir()")
test_that("ig_dir() vs nw_dir())", {
  expect_equal(net_has_isolates(ig_undir()), net_has_isolates(nw_undir()))
  })
context("net_has_isolates ig_undir() vs bridge_net_undir()")
test_that("ig_undir() vs bridge_net_undir())", {
  expect_equal(net_has_isolates(ig_undir()), net_has_isolates(bridge_net_undir()))
  })
#* bipartite ====
context("net_has_isolates ig_bip() vs nw_bip()")
test_that("ig_bip() vs nw_bip())", {
  expect_equal(net_has_isolates(ig_bip()), net_has_isolates(nw_bip()))
  })
context("net_has_isolates ig_bip() vs bridge_net_bip()")
test_that("ig_bip() vs bridge_net_bip())", {
  expect_equal(net_has_isolates(ig_bip()), net_has_isolates(bridge_net_bip()))
  })
