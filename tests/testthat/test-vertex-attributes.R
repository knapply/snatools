ig_dir <- build_test_graph("ig")
ig_undir <- build_test_graph("ig", directed = FALSE)
# ig_bip <- build_test_graph("ig", directed = FALSE, bipartite = TRUE)

nw_dir <- build_test_graph("nw")
nw_undir <- build_test_graph("nw", directed = FALSE)
# nw_bip <- build_test_graph("nw", directed = FALSE, bipartite = TRUE)

sna_net_dir <- as_sna_net(ig_dir)
sna_net_undir <- as_sna_net(ig_undir)
# sna_net_bip <- as_sna_net(ig_bip)

# context("vrt_get_attr")
# # directed ====
# test_that("ig_dir vs nw_dir)", {
#   expect_equal(vrt_get_attr(ig_dir, "node_dbl"), vrt_get_attr(nw_dir, "node_dbl"))
#   })
# test_that("ig_dir vs sna_net_dir)", {
#   expect_equal(vrt_get_attr(ig_dir, "node_dbl"), vrt_get_attr(sna_net_dir, "node_dbl"))
#   })
# # undirected ====
# test_that("ig_dir vs nw_dir)", {
#   expect_equal(vrt_get_attr(ig_undir, "node_chr"), vrt_get_attr(nw_undir, "node_chr"))
#   })
# test_that("ig_dir vs sna_net_dir)", {
#   expect_equal(vrt_get_attr(ig_undir, "node_chr"),
#                vrt_get_attr(sna_net_undir, "node_chr"))
#   })
# # bipartite ====
# # test_that("ig_dir vs nw_dir)", {
# #   expect_equal(vrt_get_attr(ig_bip, ".name"), vrt_get_attr(nw_bip, ".name"))
# #   })
# # test_that("ig_dir vs sna_net_dir)", {
# #   expect_equal(vrt_get_attr(ig_bip, ".name"), vrt_get_attr(sna_net_bip, ".name"))
# #   })
#
#
# context("vrt_get_attr_df")
# # directed ====
# test_that("ig_dir vs nw_dir)", {
#   expect_equal(vrt_get_attr_df(ig_dir), vrt_get_attr_df(nw_dir))
#   })
# # test_that("ig_dir vs sna_net_dir)", {
# #   expect_equal(vrt_get_attr_df(ig_dir), vrt_get_attr_df(sna_net_dir))
# #   })
# # undirected ====
# test_that("ig_dir vs nw_dir)", {
#   expect_equal(vrt_get_attr_df(ig_undir), vrt_get_attr_df(nw_undir))
#   })
# # test_that("ig_dir vs sna_net_dir)", {
# #   expect_equal(vrt_get_attr_df(ig_undir), vrt_get_attr_df(sna_net_undir))
# #   })
# # bipartite ====
# # test_that("ig_dir vs nw_dir)", {
#   # expect_equal(vrt_get_attr_df(ig_bip), vrt_get_attr_df(nw_bip))
#   # })
# # test_that("ig_dir vs sna_net_dir)", {
# #   expect_equal(vrt_get_attr_df(ig_bip), vrt_get_attr_df(sna_net_bip))
# #   })
