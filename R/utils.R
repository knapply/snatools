.is_symmetric <- function(x) {
  all(x == t(x))
}

# `%||%` <- function(lhs, rhs) {
#   if (is.null(lhs)) rhs else lhs
# }
#
# `%|error|%` <- function(lhs, error_message) {
#   if (is.null) stop(error_message, call. = FALSE) else lhs
# }

.nap_int <- function(.x, .f, ..., nm = TRUE) {
  vapply(.x, .f, FUN.VALUE = integer(1L), ..., USE.NAMES = nm)
}
.nap_dbl <- function(.x, .f, ..., nm = TRUE) {
  vapply(.x, .f, FUN.VALUE = double(1L), ..., USE.NAMES = nm)
}
.nap_chr <- function(.x, .f, ..., nm = TRUE) {
  vapply(.x, .f, FUN.VALUE = character(1L), ..., USE.NAMES = nm)
}
.nap_lgl <- function(.x, .f, ..., nm = TRUE) {
  vapply(.x, .f, FUN.VALUE = logical(1L), ..., USE.NAMES = nm)
}

# .compact <- function(.x) {
#   .x[
#     which(
#       as.logical(unlist(lapply(.x, length), use.names = FALSE)),
#       useNames = FALSE)
#   ]
# }

# .all_same <- function(.x) {
#   stopifnot(is.atomic(.x))
#   length(.x[!duplicated(.x)]) == 1L
# }

.stop <- function(...) {
  stop(..., call. = FALSE)
}

.apply_rows <- function(.x, .f, ...)  {
  t(apply(.x, MARGIN = 1L, .f, ...))
}

.is_empty <- function(.x) {
  length(.x) == 0L
}

