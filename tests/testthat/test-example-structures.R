test_that("example_adj_mat() works as expected", {
  expect_true(
    .is_symmetric(example_adj_mat())
  )
  expect_true(
    .is_symmetric(example_adj_mat(loops = TRUE))
  )
  expect_false(
    .is_symmetric(example_adj_mat(directed = TRUE))
  )
  expect_false(
    .is_symmetric(example_adj_mat(directed = TRUE, loops = TRUE))
  )
})

test_that("example_igraph()/example_network() return identical matrices", {
  expect_identical(
    igraph::as_adjacency_matrix(
      example_igraph(),
      sparse = FALSE
    ),
    network::as.matrix.network.adjacency(
      example_network()
    )
  )

  expect_identical(
    igraph::as_adjacency_matrix(
      example_igraph(loops = TRUE),
      sparse = FALSE
    ),
    network::as.matrix.network.adjacency(
      example_network(loops = TRUE)
    )
  )

  expect_identical(
    igraph::as_adjacency_matrix(
      example_igraph(directed = TRUE),
      sparse = FALSE
    ),
    network::as.matrix.network.adjacency(
      example_network(directed = TRUE)
    )
  )

  expect_identical(
    igraph::as_adjacency_matrix(
      example_igraph(directed = TRUE, loops = TRUE),
      sparse = FALSE
    ),
    network::as.matrix.network.adjacency(
      example_network(directed = TRUE, loops = TRUE)
    )
  )
})

test_that("example_igraph()/example_network() return identical attributes", {
  expect_identical(
    igraph::vertex_attr(example_igraph(), "color"),
    network::get.vertex.attribute(example_network(), "color")
  )

  expect_identical(
    igraph::vertex_attr(example_igraph(), "size"),
    network::get.vertex.attribute(example_network(), "size")
  )

  expect_identical(
    igraph::vertex_attr(example_igraph(), "list_attr"),
    network::get.vertex.attribute(example_network(), "list_attr",
                                  unlist = FALSE) # TODO don't make same mistake
  )
})
