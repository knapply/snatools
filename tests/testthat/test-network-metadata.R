ig_dir <- build_test_graph("ig")
ig_undir <- build_test_graph("ig", directed = FALSE)
ig_bip <- build_test_graph("ig", directed = FALSE, bipartite = TRUE)

nw_dir <- build_test_graph("nw")
nw_undir <- build_test_graph("ig", directed = FALSE)
nw_bip <- build_test_graph("nw", directed = FALSE, bipartite = TRUE)

sna_net_dir <- as_sna_net(ig_dir)
sna_net_undir <- as_sna_net(ig_undir)
sna_net_bip <- as_sna_net(ig_bip)

# net_is_directed ====
context("net_is_directed")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_is_directed(ig_dir), net_is_directed(nw_dir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_is_directed(ig_dir), net_is_directed(sna_net_dir))
  })
#* undirected ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_is_directed(ig_undir), net_is_directed(nw_undir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_is_directed(ig_undir), net_is_directed(sna_net_undir))
  })
#* bipartite ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_is_directed(ig_bip), net_is_directed(nw_bip))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_is_directed(ig_bip), net_is_directed(sna_net_bip))
  })

# net_is_bipartite ====
context("net_is_bipartite")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_is_bipartite(ig_dir), net_is_bipartite(nw_dir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_is_bipartite(ig_dir), net_is_bipartite(sna_net_dir))
  })
#* undirected ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_is_bipartite(ig_undir), net_is_bipartite(nw_undir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_is_bipartite(ig_undir), net_is_bipartite(sna_net_undir))
  })
#* bipartite ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_is_bipartite(ig_bip), net_is_bipartite(nw_bip))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_is_bipartite(ig_bip), net_is_bipartite(sna_net_bip))
  })


# edge counts ====
context("edge counts")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_count_edges(ig_dir), net_count_edges(nw_dir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_count_edges(ig_dir), net_count_edges(sna_net_dir))
  })
#* undirected ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_count_edges(ig_undir), net_count_edges(nw_undir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_count_edges(ig_undir), net_count_edges(sna_net_undir))
  })
#* bipartite ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_count_edges(ig_bip), net_count_edges(nw_bip))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_count_edges(ig_bip), net_count_edges(sna_net_bip))
  })


# vertex counts ====
context("vertex counts")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_count_vertices(ig_dir), net_count_vertices(nw_dir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_count_vertices(ig_dir), net_count_vertices(sna_net_dir))
  })
#* undirected ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_count_vertices(ig_undir), net_count_vertices(nw_undir))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_count_vertices(ig_undir), net_count_vertices(sna_net_undir))
  })
#* bipartite ====
test_that("ig_dir vs nw_dir)", {
  expect_equal(net_count_vertices(ig_bip), net_count_vertices(nw_bip))
  })
test_that("ig_dir vs sna_net_dir)", {
  expect_equal(net_count_vertices(ig_bip), net_count_vertices(sna_net_bip))
  })