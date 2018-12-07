ig_dir <- function() snatools:::build_test_graph("ig", isolate = TRUE)
ig_undir <- function() snatools:::build_test_graph("ig", directed = FALSE, isolate = TRUE)
ig_bip <- function() snatools:::build_test_graph("ig", directed = FALSE, bipartite = TRUE)

nw_dir <- function() snatools:::build_test_graph("nw", isolate = TRUE)
nw_undir <- function() snatools:::build_test_graph("nw", directed = FALSE, isolate = TRUE)
nw_bip <- function() snatools:::build_test_graph("nw", directed = FALSE, bipartite = TRUE)

bridge_net_dir <- function() as_bridge_net(ig_dir())
bridge_net_undir <- function() as_bridge_net(ig_undir())
bridge_net_bip <- function() as_bridge_net(ig_bip())

# net_attr_names() ====
context("net_attr_names")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_equal(net_attr_names(ig_dir()), net_attr_names(nw_dir()))
  })
test_that("ig_dir vs bridge_net_dir", {
  expect_equal(net_attr_names(ig_dir()), net_attr_names(bridge_net_dir()))
  })
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_equal(net_attr_names(ig_undir()), net_attr_names(nw_undir()))
  })
test_that("ig_undir vs bridge_net_undir", {
  expect_equal(net_attr_names(ig_undir()), net_attr_names(bridge_net_undir()))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip", {
  expect_equal(net_attr_names(ig_bip()), net_attr_names(nw_bip()))
  })
test_that("ig_bip vs bridge_net_bip", {
  expect_equal(net_attr_names(ig_bip()), net_attr_names(bridge_net_bip()))
  })

# net_get_attr() ====
context("net_attr_names")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_equal(net_get_attr(ig_dir(), "graph_dbl"), net_get_attr(nw_dir(), "graph_dbl"))
  })
test_that("ig_dir vs bridge_net_dir", {
  expect_equal(net_get_attr(ig_dir(), "graph_dbl"), 
               net_get_attr(bridge_net_dir(), "graph_dbl"))
  })
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_equal(net_get_attr(ig_undir(), "graph_dbl"), net_get_attr(nw_undir(), "graph_dbl"))
  })
test_that("ig_undir vs bridge_net_undir", {
  expect_equal(net_get_attr(ig_undir(), "graph_dbl"),
               net_get_attr(bridge_net_dir(), "graph_dbl"))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip", {
  expect_equal(net_get_attr(ig_bip(), "graph_dbl"), net_get_attr(nw_bip(), "graph_dbl"))
  })
test_that("ig_bip vs bridge_net_bip", {
  expect_equal(net_get_attr(ig_bip(), "graph_dbl"),
               net_get_attr(bridge_net_bip(), "graph_dbl"))
  })

# net_attrs_to_list() ====
context("net_attrs_to_list")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_equal(net_attrs_to_list(ig_dir()), net_attrs_to_list(nw_dir()))
  })
test_that("ig_dir vs bridge_net_dir", {
  expect_equal(net_attrs_to_list(ig_dir()), net_attrs_to_list(bridge_net_dir()))
  })
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_equal(net_attrs_to_list(ig_undir()), net_attrs_to_list(nw_undir()))
  })
test_that("ig_undir vs bridge_net_undir", {
  expect_equal(net_attrs_to_list(ig_undir()), net_attrs_to_list(bridge_net_dir()))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip", {
  expect_equal(net_attrs_to_list(ig_bip()), net_attrs_to_list(nw_bip()))
  })
test_that("ig_bip vs bridge_net_bip", {
  expect_equal(net_attrs_to_list(ig_bip()), net_attrs_to_list(bridge_net_bip()))
  })
