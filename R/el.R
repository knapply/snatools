.rep_as_el <- function(x, ...) {
  UseMethod(".rep_as_el")
}

#' @importFrom igraph as_edgelist
.rep_as_el.igraph <- function(x, ...) {
  as_edgelist(x, names = FALSE)
}

.rep_as_el.network <- function(x, reorder = TRUE, ...) {
  nal <- lapply(x[["mel"]], `[[`, "na")

  cbind(
    .nap_dbl(x[["mel"]], `[[`, "outl", nm = FALSE),
    .nap_dbl(x[["mel"]], `[[`, "outl", nm = FALSE)
  )

  outl <- .nap_dbl(x[["mel"]], `[[`, "outl", nm = FALSE)
  inl <- .nap_dbl(x[["mel"]], `[[`, "inl", nm = FALSE)

  if (net_is_directed(x)) {
    out <- cbind(outl, inl, deparse.level = 0L)
  } else {
    out <- cbind(inl, outl, deparse.level = 0L)
  }

  # if (reorder) {
  #   out[] <- out[order(out[, 1L]), ]
  # }

  out
}

