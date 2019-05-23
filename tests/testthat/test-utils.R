test_that('"default"ers work', {
  # `%||%` =============================================================================
  expect_true(
    NULL %||% TRUE
  )
  expect_false(
    FALSE %||% TRUE
  )
  expect_identical(
    NA %||% TRUE,
    NA
  )
  # `%{}%` ============================================================================= 
  expect_true(
    NULL %{}% TRUE
  )
  expect_true(
    integer() %{}% TRUE
  )
  expect_false(
    FALSE %{}% TRUE
  )
  expect_equal(
    1 %{}% TRUE,
    1
  )
  # `%{NA}%` ===========================================================================
  expect_null(
    NULL %{NA}% FALSE
  )
  expect_false(
    NA %{NA}% FALSE
  )
})

test_that('`is_*()` work', {
  # `is_empty()` =======================================================================
  expect_true(
    is_empty(NULL)
  )
  expect_true(
    is_empty(integer())
  )
  expect_true(
    is_empty(list())
  )
  expect_false(
    is_empty(list(NULL))
  )
})