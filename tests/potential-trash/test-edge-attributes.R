ig_dir <- function() build_test_graph("ig")
ig_undir <- function() build_test_graph("ig", directed = FALSE)
ig_bip <- function() build_test_graph("ig", directed = FALSE, bipartite = TRUE)

nw_dir <- function() build_test_graph("nw")
nw_undir <- function() build_test_graph("nw", directed = FALSE)
nw_bip <- function() build_test_graph("nw", directed = FALSE, bipartite = TRUE)

# sna_net_dir <- as_bridge_net(ig_dir())
# sna_net_undir <- as_bridge_net(ig_undir())

# edge attribute names ====
#* directed ====
test_that("edg_attr_names() ig_dir() vs nw_dir()", {
  expect_identical(edg_attr_names(ig_dir()),
                   edg_attr_names(nw_dir()))
  })
#* undirected ====
test_that("edg_attr_names() ig_undir() vs nw_undir()", {
  expect_identical(edg_attr_names(ig_undir()),
                   edg_attr_names(nw_undir()))
  })
#* bipartite ====
test_that("edg_attr_names() ig_bip() vs nw_bip())", {
  expect_identical(edg_attr_names(ig_bip()),
                   edg_attr_names(nw_bip()))
  })


# get edge attribute ====
#* directed ====
test_that("edg_get_attr() ig_dir() vs nw_dir()", {
  expect_identical(edg_get_attr(ig_dir(), "edge_dbl"),
                   edg_get_attr(nw_dir(), "edge_dbl"))
  })
#* undirected ====
test_that("edg_get_attr() ig_undir() vs nw_undir()", {
  expect_identical(edg_get_attr(ig_undir(), "edge_chr"),
                   edg_get_attr(nw_undir(), "edge_chr"))
  })
#* bipartite ====
test_that("edg_get_attr() ig_bip() vs nw_bip())", {
  expect_identical(edg_get_attr(ig_bip(), "edge_int"),
                   edg_get_attr(nw_bip(), "edge_int"))
  })


# edge attribute data frame
context("edg_to_df() directed")
#* directed ====
test_that("ig_dir() vs nw_dir()", {
  expect_identical(edg_to_df(ig_dir()),
                   edg_to_df(nw_dir()))
  })
#* undirected ====
context("edg_to_df() undirected")
test_that("ig_undir() vs nw_undir()", {
  expect_identical(edg_to_df(ig_undir()),
                   edg_to_df(nw_undir()))
  })
#* bipartite ====
context("edg_to_df() bipartite")
test_that("ig_bip() vs nw_bip())", {
  expect_identical(edg_to_df(ig_bip()),
                   edg_to_df(nw_bip()))
  })
