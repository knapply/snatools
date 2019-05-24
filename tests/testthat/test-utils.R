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
  # `%{T}%` ===========================================================================
  expect_false(
    TRUE %{T}% FALSE
  )
  expect_false(
    FALSE %{T}% TRUE
  )
  # `%{F}%` ===========================================================================
  expect_true(
    FALSE %{F}% TRUE
  )
  expect_true(
    TRUE %{F}% FALSE
  )
  
  
})

test_that('`is_*()`/`are_*()` work', {
  # `all_equal()` =======================================================================
  expect_true(
    all_equal(1, 1L, 1.0, as.integer(TRUE))
  )
  expect_true(
    all_equal(NULL, NULL)
  )
  expect_false(
    all_equal(NULL, NA)
  )
  # `all_identical()` =======================================================================
  expect_true(
    all_identical(1L, 1L, as.integer(TRUE))
  )
  expect_true(
    all_identical(NULL, NULL)
  )
  expect_false(
   all_identical(NULL, NA)
  )
  # `is_empty()` ========================================================================
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