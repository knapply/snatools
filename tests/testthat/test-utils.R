context("internal utility functions")

test_that("utils work as expected", {
  expect_identical(
    NULL %||% "NULL replacement", "NULL replacement"
    )
  expect_identical(
    character(0) %{}% "empty replacement", "empty replacement"
    )
  expect_identical(
    list() %{}% "empty replacement", "empty replacement"
    )
  expect_identical(
    drop_nulls(list(1, NULL, 2, NULL, 3, NULL, 4)), list(1, 2, 3, 4)
    )
  # expect_true(
  #   is_null(NULL)
  # )
  # expect_false(
  #   is_null(list(NULL))
  # )
  expect_true(
    is_scalar(1)
  )
  expect_false(
    is_scalar(1:3)
  )
  expect_false(
    is_scalar(list(1, 2, 3))
  )
  expect_true(
    is_named(c(a_name = 1))
  )
  expect_false(
    is_named(`names<-`(1, NA_character_))
  )
})

