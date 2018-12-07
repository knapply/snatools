context("vertex attributes")

ig_vert_attrs <- tibble::tibble(name = letters[1:10],
                                foo = LETTERS[1:10],
                                bar = rep(c(TRUE, FALSE), 5))

edge_attrs <- tibble::tibble(.ego = c(5, 9, 10, 10, 10, 10, 5, 3, 6,
                                      3, 2, 2, 3, 1, 6, 3, 2, 1, 6, 6),
                             .alter = c(2, 5, 8, 7, 10, 1, 4, 3, 3, 3, 2,
                                        6, 10, 10, 3, 7, 10, 9, 8, 2),
                             foo = LETTERS[1:20],
                             bar = rep(c(TRUE, FALSE), 10))
ig <- igraph::graph_from_edgelist(cbind(edge_attrs$.ego, edge_attrs$.alter))
igraph::vertex_attr(ig) <- ig_vert_attrs
igraph::edge_attr(ig) <- edge_attrs

nw_vert_attrs <- ig_vert_attrs
names(nw_vert_attrs)[names(nw_vert_attrs) == "name"] <- "vertex.names"

nw <- network::as.network.matrix(cbind(edge_attrs$.ego, edge_attrs$.alter))
network::set.vertex.attribute(nw, colnames(nw_vert_attrs), nw_vert_attrs)
network::set.edge.attribute(nw, colnames(edge_attrs), edge_attrs)

# vrt_attr_names() ====
test_that("attribute name extraction works", {
# igraph
  expect_identical(
    vrt_attr_names(ig),
    colnames(ig_vert_attrs)
  )
# network
  expect_identical(
    vrt_attr_names(nw),
    colnames(nw_vert_attrs)
  )
})

# vrt_get_attr() ====
test_that("specific attribute extraction works", {
# igraph
  expect_identical(
    vrt_get_attr(ig, "foo"),
    ig_vert_attrs$foo
  )
  expect_error(
    vrt_get_attr(ig, "fake attr name")
  )
# network
  expect_identical(
    vrt_get_attr(nw, "bar"),
    nw_vert_attrs$bar
  )
  expect_error(
    vrt_get_attr(nw, "fake attr name")
  )
})

# vrt_get_names() ====
test_that("vertex name extraction works", {
# igraph
  expect_identical(
    vrt_get_names(ig),
    ig_vert_attrs$name
  )
# network
  expect_identical(
    vrt_get_names(nw),
    nw_vert_attrs$vertex.names
  )
})

ig_nulls <- igraph::graph_from_edgelist(cbind(edge_attrs$.ego,
                                              edge_attrs$.alter))
# network should never have NULL attributes... but it still happens
nw_nulls <- network::as.network.matrix(cbind(edge_attrs$.ego,
                                             edge_attrs$.alter),
                                       matrix.type = "edgelist")
network::delete.vertex.attribute(nw_nulls, "vertex.names")

# test empty attributes =====
test_that("empty attributes return NULL", {
# igraph
  expect_null(
    vrt_attr_names(ig_nulls)
    )
  expect_identical(
    vrt_get_names(ig_nulls),
    seq_len(igraph::vcount(ig_nulls))
  )
# network
  expect_null(
    vrt_attr_names(nw_nulls)
    )
  expect_identical(
    vrt_get_names(nw_nulls),
    seq_len(nw_nulls[["gal"]][["n"]])
  )
})
