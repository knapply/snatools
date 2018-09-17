# identical as_sna_nets from empty graphs ====
context("converting empty graphs to `sna_net`s")
#* empty directed ====
test_that("as_sna_net(empty ig_dir) vs as_sna_net(empty nw_dir)", {
  expect_identical(as_sna_net(build_test_graph("ig", n_nodes = 0)),
                   as_sna_net(build_test_graph("nw", n_nodes = 0)))
  })
#* empty undirected ====
test_that("as_sna_net(empty ig_undir) vs as_sna_net(empty nw_undir)", {
  expect_identical(as_sna_net(build_test_graph("ig", n_nodes = 0, directed = FALSE)),
                   as_sna_net(build_test_graph("nw", n_nodes = 0, directed = FALSE)))
  })
#* empty directed bipartite ===
test_that("as_sna_net(empty ig_dir_bip) vs as_sna_net(empty nw_dir_bip)", {
  expect_identical(as_sna_net(build_test_graph("ig", n_nodes = 0, bipartite = TRUE)),
                   as_sna_net(build_test_graph("nw", n_nodes = 0, bipartite = TRUE)))
  })
#* empty directed bipartite ===
test_that("as_sna_net(empty ig_undir_bip) vs as_sna_net(empty nw_undir_bip)", {
  expect_identical(as_sna_net(build_test_graph("ig", n_nodes = 0, 
                                               directed = FALSE, bipartite = TRUE)),
                   as_sna_net(build_test_graph("nw", n_nodes = 0, 
                                               directed = FALSE, bipartite = TRUE)))
  })

# identical as_sna_nets from non-empty graphs ====
context("converting non-empty graphs to `sna_net`s")
#* directed ====
test_that("as_sna_net(ig_dir) vs as_sna_net(nw_dir)", {
  expect_identical(as_sna_net(build_test_graph("ig")),
                   as_sna_net(build_test_graph("nw")))
  })
#* undirected ====
test_that("as_sna_net(ig_undir) vs as_sna_net(nw_undir)", {
  expect_identical(as_sna_net(build_test_graph("ig", directed = FALSE)),
                   as_sna_net(build_test_graph("nw", directed = FALSE)))
  })
#* directed bipartite ===
test_that("as_sna_net(ig_dir_bip) vs as_sna_net(nw_dir_bip)", {
  expect_identical(as_sna_net(build_test_graph("ig", bipartite = TRUE)),
                   as_sna_net(build_test_graph("nw", bipartite = TRUE)))
  })
#* directed bipartite ===
test_that("as_sna_net(ig_undir_bip) vs as_sna_net(nw_undir_bip)", {
  expect_identical(as_sna_net(build_test_graph("ig", 
                                               directed = FALSE, bipartite = TRUE)),
                   as_sna_net(build_test_graph("nw", 
                                               directed = FALSE, bipartite = TRUE)))
  })

# igraph to network vertices match control network ====
context("`igraph` %>% `network` vertices match control")
#* empty directed ====
test_that("`as_network(ig_dir)` vertices match `nw_dir` vertices", {
  expect_identical(vrt_get_attr_df(as_network(build_test_graph("ig"))),
                   vrt_get_attr_df(build_test_graph("nw")))
  })
#* empty directed ====
test_that("`as_network(ig_undir)` vertices match `nw_undir` vertices", {
  expect_identical(vrt_get_attr_df(as_network(build_test_graph("ig", directed = FALSE))),
                   vrt_get_attr_df(build_test_graph("nw", directed = FALSE)))
  })
#* directed bipartite ===
test_that("`as_network(ig_dir_bip)` vertices match `nw_dir_bip` vertices", {
  expect_identical(vrt_get_attr_df(as_network(build_test_graph("ig", bipartite = TRUE))),
                   vrt_get_attr_df(build_test_graph("nw", bipartite = TRUE)))
  })
#* undirected bipartite ===
test_that("`as_network(ig_undir_bip)` vertices match `nw_undir_bip` vertices", {
  expect_identical(vrt_get_attr_df(as_network(build_test_graph("ig", 
                                                               directed = FALSE,
                                                               bipartite = TRUE))),
                   vrt_get_attr_df(build_test_graph("nw", 
                                                    directed = FALSE,
                                                    bipartite = TRUE)))
  })

# network to igraph vertices match control igraph ====
context("`network` %>% `igraph` vertices match control")
#* empty directed ====
test_that("`as_igraph(nw_dir)` vertices match `ig_dir` vertices", {
  expect_identical(vrt_get_attr_df(as_igraph(build_test_graph("nw"))),
                   vrt_get_attr_df(build_test_graph("ig")))
  })
#* empty directed ====
test_that("`as_igraph(nw_undir)` vertices match `ig_undir` vertices", {
  expect_identical(vrt_get_attr_df(as_igraph(build_test_graph("nw", directed = FALSE))),
                   vrt_get_attr_df(build_test_graph("ig", directed = FALSE)))
  })
#* directed bipartite ===
test_that("`as_network(ig_dir_bip)` vertices match `nw_dir_bip` vertices", {
  expect_identical(vrt_get_attr_df(as_igraph(build_test_graph("nw", bipartite = TRUE))),
                   vrt_get_attr_df(build_test_graph("ig", bipartite = TRUE)))
  })
#* undirected bipartite ===
test_that("`as_network(ig_undir_bip)` vertices match `nw_undir_bip` vertices", {
  expect_identical(vrt_get_attr_df(as_igraph(build_test_graph("nw", 
                                                               directed = FALSE,
                                                               bipartite = TRUE))),
                   vrt_get_attr_df(build_test_graph("ig", 
                                                    directed = FALSE,
                                                    bipartite = TRUE)))
  })
