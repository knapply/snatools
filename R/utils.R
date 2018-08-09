# TODO bring in {knapply} functions or readapt them to {purrr}?

# Code defensively, or R will stab you in the back.
`%>%` <- magrittr:::pipe()

`%==%` <- function(lhs, rhs) {
  identical(lhs, rhs)
}

ifelse(NULL == "NULL", "samesies", "no more samesies")
is.null(list())
is.null(pairlist())
list(a = NULL, list(b = NULL)) %>% is.null()
list(a = NULL, list(b = NULL)) %>% unlist(use.names = TRUE) %>% is.null()
NULL %==% pairlist()

TRUE == TRUE
TRUE == 1
TRUE == 1L
TRUE %==% 1

is.na(NA) == is.na(NA_real_)
NA == NA_complex_
NA_integer_ == NA_real_
NA_integer_ == NA_real_ %>% isFALSE()
NA_integer_ == NA_real_ %>% isTRUE()
any(NA %==% NA_real_, NA_real_ %==% NA_character_, NA_integer_ %==% NA_complex_)
