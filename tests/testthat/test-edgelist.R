ig_dir <- build_test_graph("ig")
ig_undir <- build_test_graph("ig", directed = FALSE)
ig_bip <- build_test_graph("ig", directed = FALSE, bipartite = TRUE)

nw_dir <- build_test_graph("nw")
nw_undir <- build_test_graph("ig", directed = FALSE)
nw_bip <- build_test_graph("nw", directed = FALSE, bipartite = TRUE)

sna_net_dir <- as_bridge_net(ig_dir)
sna_net_undir <- as_bridge_net(ig_undir)
sna_net_bip <- as_bridge_net(ig_bip)


# raw el w/ indices ====
context("raw matrix edgelists using indices")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_identical(rep_as_edgelist(ig_dir, use_names = FALSE, leave_raw = TRUE), 
                   rep_as_edgelist(nw_dir, use_names = FALSE, leave_raw = TRUE))
  })
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_identical(rep_as_edgelist(ig_undir, use_names = FALSE, leave_raw = TRUE), 
                   rep_as_edgelist(nw_undir, use_names = FALSE, leave_raw = TRUE))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(rep_as_edgelist(ig_bip, use_names = FALSE, leave_raw = TRUE), 
                   rep_as_edgelist(nw_bip, use_names = FALSE, leave_raw = TRUE))
  })

# raw el w/ vertex names ====
context("raw matrix edgelists using names")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_identical(rep_as_edgelist(ig_dir, leave_raw = TRUE),
                   rep_as_edgelist(nw_dir, leave_raw = TRUE))
  })
#* undirected ====
test_that("ig_undir vs nw_undir)", {
  expect_identical(rep_as_edgelist(ig_undir, leave_raw = TRUE),
                   rep_as_edgelist(nw_undir, leave_raw = TRUE))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(rep_as_edgelist(ig_bip, leave_raw = TRUE), 
                   rep_as_edgelist(nw_bip, leave_raw = TRUE))
  })

# raw el w/ vertex attributes ====
context("raw matrix edgelists using vertex attributes")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_identical(rep_as_edgelist(ig_dir, vrt_attr = "node_chr", leave_raw = TRUE), 
                   rep_as_edgelist(nw_dir, vrt_attr = "node_chr", leave_raw = TRUE))
  })
#* undirected ====
test_that("ig_undir vs nw_undir)", {
  expect_identical(rep_as_edgelist(ig_undir, vrt_attr = "node_dbl", leave_raw = TRUE), 
                   rep_as_edgelist(nw_undir, vrt_attr = "node_dbl", leave_raw = TRUE))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(rep_as_edgelist(ig_bip, vrt_attr = "node_dbl", leave_raw = TRUE), 
                   rep_as_edgelist(nw_bip, vrt_attr = "node_dbl", leave_raw = TRUE))
  })


# edgelist objects w/ indices ====
context("edgelist class objects using indices")
#* directed ====
test_that("ig_dir vs nw_dir", {
  expect_identical(rep_as_edgelist(ig_dir, use_names = FALSE), 
                   rep_as_edgelist(nw_dir, use_names = FALSE))
  })
attributes(rep_as_edgelist(nw_dir, use_names = FALSE))$n_vertices %>% storage.mode()
attributes(rep_as_edgelist(ig_dir, use_names = FALSE))$n_vertices %>% storage.mode()
#* undirected ====
test_that("ig_undir vs nw_undir", {
  expect_identical(rep_as_edgelist(ig_undir, use_names = FALSE), 
                   rep_as_edgelist(nw_undir, use_names = FALSE))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(rep_as_edgelist(ig_bip, use_names = FALSE), 
                   rep_as_edgelist(nw_bip, use_names = FALSE))
  })

# edgelist class w/ vertex names ====
context("edgelist class objects using names")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_identical(rep_as_edgelist(ig_dir), 
                   rep_as_edgelist(nw_dir))
  })
#* undirected ====
test_that("ig_undir vs nw_undir)", {
  expect_identical(rep_as_edgelist(ig_undir), 
                   rep_as_edgelist(nw_undir))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(rep_as_edgelist(ig_bip), 
                   rep_as_edgelist(nw_bip))
  })

# edgelist class w/ vertex attributes ====
context("edgelist class objects using vertex attributes")
#* directed ====
test_that("ig_dir vs nw_dir)", {
  expect_identical(rep_as_edgelist(ig_dir, vrt_attr = "node_chr"), 
                   rep_as_edgelist(nw_dir, vrt_attr = "node_chr"))
  })
#* undirected ====
test_that("ig_undir vs nw_undir)", {
  expect_identical(rep_as_edgelist(ig_undir, vrt_attr = "node_dbl"), 
                   rep_as_edgelist(nw_undir, vrt_attr = "node_dbl"))
  })
#* bipartite ====
test_that("ig_bip vs nw_bip)", {
  expect_identical(rep_as_edgelist(ig_bip, vrt_attr = "node_dbl"), 
                   rep_as_edgelist(nw_bip, vrt_attr = "node_dbl"))
  })

