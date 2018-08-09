context("Build edge lists: igraph")

ig_undir <- build_test_graph("ig", direct = FALSE)
ig_dir <- build_test_graph("ig", direct = TRUE)

ig_make_el <- function(ig, use_names = FALSE) {
  out <- igraph::as_edgelist(ig, names = use_names)
  if(igraph::is_directed(ig)) {
    colnames(out) <- c("from", "to")
  } else {
    colnames(out) <- c("vert1", "vert2")
  }
  out
}

test_that("igraph: undirected unnamed el is as expected", {
  expect_equal(sna_get_edgelist(ig_undir), 
               ig_make_el(ig_undir))
})

test_that("igraph: undirected named el is as expected", {
  expect_equal(sna_get_edgelist(ig_undir, use_names = TRUE), 
               ig_make_el(ig_undir, use_names = TRUE))
})

test_that("igraph: directed unnamed el is as expected", {
  expect_equal(sna_get_edgelist(ig_undir), 
               ig_make_el(ig_undir))
})

test_that("directed igraph named el is as expected", {
  expect_equal(sna_get_edgelist(ig_dir, use_names = TRUE), 
               ig_make_el(ig_dir, use_names = TRUE))
})

context("Build edge lists: network")

nw_undir <- build_test_graph("nw", direct = FALSE)
nw_dir <- build_test_graph("nw", direct = TRUE)

nw_make_el <- function(nw, use_names = FALSE) {
  out <- network::as.matrix.network.edgelist(nw)
  attr(out, "n") <- NULL
  attr(out, "vnames") <- NULL
  if(nw$gal$directed) {
    colnames(out) <- c("from", "to")
  } else {
    temp <- out
    out[, 1] <- temp[, 2]
    out[, 2] <- temp[, 1]
    colnames(out) <- c("vert1", "vert2")
  }
  out
}

test_that("network: undirected unnamed el is as expected", {
  expect_equal(sna_get_edgelist(nw_undir), 
               nw_make_el(nw_undir))
})

test_that("network: directed unnamed el is as expected", {
  expect_equal(sna_get_edgelist(nw_dir), 
               nw_make_el(nw_dir))
})

# TODO there's no way to get a named edgelist from a `network` object?





