test_that("node_get_attr() works", {

  expect_identical(
    igraph::vertex_attr(example_igraph(), "color"),
    network::get.vertex.attribute(example_network(), "color"),
  )
  expect_identical(
    node_get_attr(example_igraph(), "color"),
    node_get_attr(example_network(), "color")
  )

  expect_identical(
    igraph::vertex_attr(example_igraph(), "size"),
    network::get.vertex.attribute(example_network(), "size")
  )
  expect_identical(
    node_get_attr(example_igraph(), "size"),
    node_get_attr(example_network(), "size")
  )

  expect_identical(
    igraph::vertex_attr(example_igraph(), "list_attr"),
    network::get.vertex.attribute(example_network(), "list_attr",
                                  unlist = FALSE)
  )

  expect_identical(
    igraph::vertex_attr(example_igraph(), name = "list_attr",
                        index = 1:3),
    node_get_attr(example_igraph(), "list_attr",
                  which_nodes = 1:3)
  )
  expect_identical(
    node_get_attr(example_igraph(), "list_attr",
                  which_nodes = 1:3),
    node_get_attr(example_network(), "list_attr",
                  which_nodes = 1:3)
  )

  expect_identical(
    igraph::vertex_attr(example_igraph(), name = "list_attr",
                        index = c(TRUE, FALSE, TRUE, FALSE, TRUE)),
    node_get_attr(example_igraph(), "list_attr",
                  which_nodes = c(TRUE, FALSE, TRUE, FALSE, TRUE))
  )
  expect_identical(
    node_get_attr(example_igraph(), "list_attr",
                  which_nodes = c(TRUE, FALSE, TRUE, FALSE, TRUE)),
    node_get_attr(example_network(), "list_attr",
                  which_nodes = c(TRUE, FALSE, TRUE, FALSE, TRUE))
  )

})


test_that("node_get_names() works", {
  expect_identical(
    node_get_names(example_network()),
    node_get_names(example_igraph())
  )


  expect_identical(
    node_get_names(
      network::delete.vertex.attribute(example_network(), "vertex.names")
    ),
    node_get_names(
      igraph::delete_vertex_attr(example_igraph(), "name")
    )
  )
})


test_that("node_attr_names() works", {
  target_ig <- c("color", "size", "x", "y", "name", "list_attr", "hetero_attr")
  target_nw <- target_ig
  target_nw[target_nw == "name"] <- "vertex.names"

  expect_true(
    all(node_attr_names(example_igraph()) %in% target_ig)
  )

  expect_true(
    all(node_attr_names(example_network()) %in% target_nw)
  )
})


test_that("node_count() catches malformed <network>s", {
  expect_error(
    node_count(
      network::delete.network.attribute(example_network(), "n")
    )
  )
})
