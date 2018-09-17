ig_dir <- build_test_graph("ig")
ig_undir <- build_test_graph("ig", directed = FALSE)
ig_bip <- build_test_graph("ig", directed = FALSE, bipartite = TRUE)

nw_dir <- build_test_graph("nw")
nw_undir <- build_test_graph("nw", directed = FALSE)
nw_bip <- build_test_graph("nw", directed = FALSE, bipartite = TRUE)

# sna_net_dir <- as_sna_net(ig_dir)
# sna_net_undir <- as_sna_net(ig_undir)

# edge attribute names ====
context("edg_attr_names()")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_identical(edg_attr_names(ig_dir),
                   edg_attr_names(nw_dir))
  })
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_identical(edg_attr_names(ig_undir),
                   edg_attr_names(nw_undir))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(edg_attr_names(ig_bip),
                   edg_attr_names(nw_bip))
  })


# get edge attribute ====
context("edg_get_attr()")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_identical(edg_get_attr(ig_dir, "edge_dbl"),
                   edg_get_attr(nw_dir, "edge_dbl"))
  })
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_identical(edg_get_attr(ig_undir, "edge_chr"),
                   edg_get_attr(nw_undir, "edge_chr"))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(edg_get_attr(ig_bip, "edge_int"),
                   edg_get_attr(nw_bip, "edge_int"))
  })


# edge attribute data frame
context("edg_get_attr_df()")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_identical(edg_get_attr_df(ig_dir),
                   edg_get_attr_df(nw_dir))
  })
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_identical(edg_get_attr_df(ig_undir),
                   edg_get_attr_df(nw_undir))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(edg_get_attr_df(ig_bip),
                   edg_get_attr_df(nw_bip))
  })
