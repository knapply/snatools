#' Construct a graph object's mixing matrix.
#' 
#' Using a categorical vertex attribute, construct a matrix depicting network mixing
#' patterns.
#'
#' @param x An `bridge_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' @param vrt_attr `character` indicating which vertex attribute to use.
#' @param drop_loops `logical` (default: `FALSE`) indicating whether to drop loop edges 
#' prior to calculations.
#' @param leave_raw `logical` (default: `FALSE`) indicating whether the returned object
#' should be a raw `matrix` rather than a `mixing_matrix` object.
#' @param mixing_matrix An object of class `mixing_matrix`.
#' @param ... Additional arguments to be passed to or used by other methods.
#' 
#' @return `mixing_matrix` or `matrix`
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @seealso [network::mixingmatrix()]
#' 
#' @examples 
#' library(snatools)
#' 
#' data("sampson", package = "ergm")
#' 
#' samplike %>% rep_as_mixing_matrix(vrt_attr = "group")
#' 
#' samplike %>% rep_as_mixing_matrix(vrt_attr = "group", leave_raw = TRUE)
#' 
#' @export
rep_as_mixing_matrix <- function(x, vrt_attr, drop_loops = FALSE, leave_raw = FALSE) {
  validate_graph(x)
  validate_vrt_attr(x, vrt_attr)
  el <- rep_as_edgelist(x, vrt_attr = vrt_attr, leave_raw = TRUE)
  if (is.numeric(el)) {
    warning("`vrt_attr` should be a categorical vertex attribute, but it's `numeric`.")
  }
  if (drop_loops) {
    el <- unique.matrix(el)
  }
  attrs <- sort(unique(`dim<-`(el, NULL)))
  out <- table(.ego = factor(el[, 1], levels = attrs),
               .alter = factor(el[, 2], levels = attrs), dnn = NULL)
  if (leave_raw){
    class(out) <- "matrix"
    return(out)
  }
  class(out) <- c("mixing_matrix", "matrix")
  attr(out, "vrt_attr") <- vrt_attr
  out
}

#' @rdname rep_as_mixing_matrix
#' 
#' @export
is_mixing_matrix <- function(x) {
  inherits(x, "mixing_matrix")
}

#' @rdname rep_as_mixing_matrix
#' 
#' @export
print.mixing_matrix <- function(mixing_matrix, ...) {
  cat_patch("# A mixing_matrix with %s attribute categories.", nrow(mixing_matrix))
  cat("\n")
  out <- rbind(cbind(NA_integer_, mixing_matrix,
                     replicate(2L, rep(NA_integer_, nrow(mixing_matrix))), 
                     rep("|", nrow(mixing_matrix)), rowSums(mixing_matrix)),
               rep(NA_integer_, ncol(mixing_matrix) + 5L),
               c(NA_integer_, colSums(mixing_matrix), rep(NA_integer_, 4L)))
  out[is.na(out)] <- ""
  out[nrow(out) - 1L, 2:(ncol(out) - 4L)] <- "-"
  dimnames(out) <- list(.ego = c(rownames(mixing_matrix), "", "Outgoing Ties"),
                        .alter = c("", colnames(mixing_matrix), 
                                   rep("", 3L), "Incoming Ties"))
  print(out, quote = FALSE, ...)
}

#' @rdname rep_as_mixing_matrix
#' 
#' @export
as.matrix.mixing_matrix <- function(mixing_matrix) {
  mat_attrs <- c("dim", "dimnames")
  for (i in names(attributes(mixing_matrix))) {
    if (!i %in% mat_attrs) {
      attr(mixing_matrix, i) <- NULL
    }
  }
  class(mixing_matrix) <- "matrix"
  mixing_matrix
}

#' @rdname rep_as_mixing_matrix
#' 
#' @export
as.data.frame.mixing_matrix <- function(mixing_matrix, ...) {
  temp <- as.data.frame(as.matrix(mixing_matrix), stringsAsFactors = FALSE, ...)
  out <- cbind.data.frame(data.frame(`.ego` = rownames(temp), stringsAsFactors = FALSE),
                          `rownames<-`(temp, NULL), stringsAsFactors = FALSE, ...)
  `rownames<-`(out, NULL)
}






