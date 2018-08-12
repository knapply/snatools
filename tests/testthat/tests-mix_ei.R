context("E-I Index: directed")


ig_undir <- build_test_graph("ig", direct = FALSE) %>% 
  clean_graph()

test_that("ig vs other", {
  expect_true(
    round(mix_ei_index(ig_undir, "node_character"), 5) %==% round(isnar::ei(ig_undir, "node_character"), 5)
    )
})

test_that("ig to nw vs other", {
  expect_true(
    round(mix_ei_index(as_network(ig_undir), "node_character"), 5) %==% round(isnar::ei(ig_undir, "node_character"), 5)
    )
})

ig_dir <- build_test_graph("ig") %>% 
  clean_graph()

context("E-I Index: undirected")

test_that("ig vs other", {
  expect_true(
    round(mix_ei_index(ig_dir, "node_character"), 5) %==% round(isnar::ei(ig_dir, "node_character"), 5)
    )
})

test_that("ig to nw vs other", {
  expect_true(
    round(mix_ei_index(as_network(ig_dir), "node_character"), 5) %==% round(isnar::ei(ig_dir, "node_character"), 5)
    )
})
