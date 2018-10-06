# ig_dir <- function() snatools:::build_test_graph("ig", isolate = TRUE)
# ig_undir <- function() snatools:::build_test_graph("ig", directed = FALSE, isolate = TRUE)
# ig_bip <- function() snatools:::build_test_graph("ig", directed = FALSE, bipartite = TRUE)
# 
# nw_dir <- function() snatools:::build_test_graph("nw", isolate = TRUE)
# nw_undir <- function() snatools:::build_test_graph("nw", directed = FALSE, isolate = TRUE)
# nw_bip <- function() snatools:::build_test_graph("nw", directed = FALSE, bipartite = TRUE)
# 
# bridge_net_dir <- function() as_bridge_net(ig_dir())
# bridge_net_undir <- function() as_bridge_net(ig_undir())
# bridge_net_bip <- function() as_bridge_net(ig_bip())
# 
# # vrt_get_attr ====
# context("vrt_get_attr")
# #* directed ====
# test_that("ig_dir vs nw_dir)", {
#   expect_identical(vrt_get_attr(ig_dir(), "node_dbl"), vrt_get_attr(nw_dir(), "node_dbl"))
#   })
# test_that("ig_dir vs bridge_net_dir)", {
#   expect_identical(vrt_get_attr(ig_dir(), "node_dbl"), vrt_get_attr(bridge_net_dir(), "node_dbl"))
#   })
# #* undirected ====
# test_that("ig_undir vs nw_undir)", {
#   expect_identical(vrt_get_attr(ig_undir(), "node_chr"), vrt_get_attr(nw_undir(), "node_chr"))
#   })
# test_that("ig_undir vs bridge_net_undir)", {
#   expect_identical(vrt_get_attr(ig_undir(), "node_chr"),
#                    vrt_get_attr(bridge_net_undir(), "node_chr"))
#   })
# #* bipartite ====
# test_that("ig_bip vs nw_bip)", {
#   expect_identical(vrt_get_attr(ig_bip(), "name"), vrt_get_attr(nw_bip(), "vertex.names"))
#   })
# test_that("ig_bip vs bridge_net_bip)", {
#   expect_identical(vrt_get_attr(ig_bip(), "node_dbl"), vrt_get_attr(bridge_net_bip(), "node_dbl"))
#   })
# 
# # vrt_to_df() ====
# context("vrt_to_df")
# #* directed ====
# test_that("ig_dir vs nw_dir)", {
#   expect_identical(vrt_to_df(ig_dir()), vrt_to_df(nw_dir()))
#   })
# test_that("ig_dir vs bridge_net_dir)", {
#   expect_identical(vrt_to_df(ig_dir()), vrt_to_df(bridge_net_dir()))
#   })
# #* undirected ====
# test_that("ig_undir vs nw_undir)", {
#   expect_identical(vrt_to_df(ig_undir()), vrt_to_df(nw_undir()))
#   })
# test_that("ig_dir vs sna_net_dir)", {
#   expect_equal(vrt_to_df(ig_undir()), vrt_to_df(bridge_net_undir()))
#   })
# #* bipartite ====
# test_that("ig_bip vs nw_bip)", {
#   expect_identical(vrt_to_df(ig_bip()), vrt_to_df(nw_bip()))
# })
# test_that("ig_dir vs sna_net_dir)", {
#   expect_identical(vrt_to_df(ig_bip()), vrt_to_df(bridge_net_bip()))
#   })
