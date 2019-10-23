test_that(".rep_as_el() works", {
  expect_identical(
    .rep_as_el(example_igraph()),
    .rep_as_el(example_network())
  )

  expect_identical(
    .rep_as_el(example_igraph(directed = TRUE)),
    .rep_as_el(example_network(directed = TRUE))
  )
})
#
#
# nw_as_el <- function(x) {
#   out <- network::as.matrix.network.edgelist(x)
#
#   to_rm <- setdiff(names(attributes(out)), "dim")
#   attributes(out)[to_rm] <- NULL
#   out
# }
#
#
# example_adj_mat(directed = TRUE)
# all(
#   .rep_as_el(example_igraph(directed = TRUE)) ==
#     .rep_as_el(example_network(directed = TRUE))
# )
#
# nw_el <- .rep_as_el(example_network(directed = TRUE))
# nw_el[order(nw_el[, 1]), ]
