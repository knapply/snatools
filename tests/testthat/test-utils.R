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

test_that('`.all_equal()`/`all_identical()', {
  # `all_equal()` =======================================================================
  expect_true(
    .all_equal(1, 1L, 1.0, as.integer(TRUE))
  )
  expect_true(
    .all_equal(NULL, NULL)
  )
  expect_false(
    .all_equal(NULL, NA)
  )
  # `all_identical()` =======================================================================
  expect_true(
    .all_identical(1L, 1L, as.integer(TRUE))
  )
  expect_true(
    .all_identical(NULL, NULL)
  )
  expect_false(
    .all_identical(NULL, NA)
  )
})

test_that('`.is_empty()` works', {
  # `is_empty()` ========================================================================
  expect_true(
    .is_empty(NULL)
  )
  expect_true(
    .is_empty(integer())
  )
  expect_true(
    .is_empty(list())
  )
  expect_false(
    .is_empty(list(NULL))
  )
})


test_that('`.is_symmetric()` works', {
  sym_target <- matrix(
    c(1, 1, 1,
      1, 0, 1,
      1, 1 ,1), 
    nrow = 3L, ncol = 3L, byrow = TRUE)
  
  asym_target <- matrix(
    c(1, 1, 1,
      1, 0, 1,
      0, 1 ,0), 
    nrow = 3L, ncol = 3L, byrow = TRUE)
  
  expect_true(
    .is_symmetric(sym_target)
  )
  
  expect_false(
    .is_symmetric(asym_target)
  )
})


