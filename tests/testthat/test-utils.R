# default-ers ==========================================================================
test_that('"default"ers work', {
  # %||% ===============================================================================
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
  # %{}% ===============================================================================
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
  # %{NA}% =============================================================================
  expect_null(
    NULL %{NA}% FALSE
  )
  expect_false(
    NA %{NA}% FALSE
  )
  # %{T}% ==============================================================================
  expect_false(
    TRUE %{T}% FALSE
  )
  expect_false(
    FALSE %{T}% TRUE
  )
  # %{F}% ==============================================================================
  expect_true(
    FALSE %{F}% TRUE
  )
  expect_true(
    TRUE %{F}% FALSE
  )
  
  
})

# .all_*() =============================================================================
test_that('.all_equal()/all_identical()', {
  #* all_equal() =======================================================================
  expect_true(
    .all_equal(1, 1L, 1.0, as.integer(TRUE))
  )
  expect_true(
    .all_equal(NULL, NULL)
  )
  expect_false(
    .all_equal(NULL, NA)
  )
  expect_false(
    .all_equal(NULL, NA, matrix())
  )
  #* all_identical() ===================================================================
  expect_true(
    .all_identical(1L, 1L, as.integer(TRUE))
  )
  expect_true(
    .all_identical(NULL, NULL)
  )
  expect_false(
    .all_identical(NULL, NA)
  )
  expect_false(
    .all_identical(NULL, NA, matrix())
  )
})

# vector .is_*() =======================================================================
test_that('.is_*() works', {
  #* .is_empty() =======================================================================
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
  #* .is_scalar() ======================================================================
  expect_true(
    .is_scalar(1)
  )
  expect_false(
    .is_scalar(c(1, 2))
  )
  #* .is_scalar_chr() ==================================================================
  expect_true(
    .is_scalar_chr("a")
  )
  expect_false(
    .is_scalar(c("a", "b"))
  )
})

# matrix .is_*() =======================================================================
test_that('.is_symmetric() works', {
  #* .is_symmetric() ===================================================================
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

# .map_*() =============================================================================
test_that('.map_*() works', {
  #* .map() =============================================================================
  expect_equal(
      .map(list(list(a = 1), list(b = 2), list(c = 3)), names),
      list("a", "b", "c")
  )
  #* .map_chr() =========================================================================
  expect_equal(
    # .all_equal(
      .map_chr(list(list(a = 1), list(b = 2), list(c = 3)), names),
      c("a", "b", "c")
    # )
  )
  #* .map_num() ========================================================================
  expect_equal(
    .map_num(list(list(a = 1), list(a = 2), list(a = 3)), `[[`, "a"),
    c(1, 2, 3)
  )
  #* .map_dbl() ========================================================================
  expect_identical(
    .map_dbl(list(list(a = 1), list(a = 2), list(a = 3)), "a"),
    c(1, 2, 3)
  )
  #* .map_int() ========================================================================
  expect_identical(
    .map_int(list(list(a = 1L), list(a = 2L), list(a = 3L)), 1),
    c(1L, 2L, 3L)
  )
  #* .map_which() ======================================================================
  expect_equal(
    .map_which(list(list(a = 1), list(b = 2), NULL), .is_empty),
    3L
  )
})

# .flatten_*() ==========================================================================
test_that('.flatten_*() works', {
  #* .flatten() =========================================================================
  expect_equal(
    .flatten(
      list(list(a = 1), list(b = 2), list(c = 3))
    ),
    list(a = 1, b = 2, c = 3)
  )
  #* .flatten_chr() ====================================================================
  expect_error(
    .flatten_chr(
      list(a = 1, b = 2, list(c = 3), list(d = 4))
    ),
    regexp = "Can't coerce the following elements from `list` to `character`: 3, 4"
  )
  expect_equal(
    .flatten_chr(
      list(1, 2, 3)
    ),
    c("1", "2", "3")
  )
})

# throwers =============================================================================
test_that('throwers works', {
  message_text <- "test"
  # .message() =========================================================================
  expect_message(
    .message("{message_text}"),
    "test"
  )
  # .stop() ============================================================================
  expect_error(
    .stop("{message_text}"),
    "test"
  )
})

