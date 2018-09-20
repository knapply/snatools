#' Construct a graph object's mixing matrix.
#' 
#' Using a categorical vertex attribute, construct a matrix depicting network mixing
#' patterns.
#'
#' @param x An `sna_net`, `igraph`, `network`, `tbl_graph`, or `edgelist` object.
#' @param vrt_attr `character` indicating which vertex attribute to use.
#' @param drop_loops `logical` (default: `FALSE`) indicating whether to drop loop edges 
#' prior to calculations.
#' @param leave_raw `logical` (default: `FALSE`) indicating whether the returned object
#' should be a raw `matrix` rather than a `mixing_matrix` object.
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
print.mixing_matrix <- function(x, ...) {
  cat_patch("# A mixing_matrix with %s attribute categories.", nrow(x))
  cat("\n")
  out <- rbind(cbind(NA_integer_, x, replicate(2L, rep(NA_integer_, nrow(x))), 
                     rep("|", nrow(x)), rowSums(x)),
               rep(NA_integer_, ncol(x) + 5L),
               c(NA_integer_, colSums(x), rep(NA_integer_, 4L)))
  out[is.na(out)] <- ""
  out[nrow(out) - 1L, 2:(ncol(out) - 4L)] <- "-"
  dimnames(out) <- list(.ego = c(rownames(x), "", "Outgoing Ties"),
                        .alter = c("", colnames(x), rep("", 3L), "Incoming Ties"))
  print(out, quote = FALSE, ...)
}

#' @rdname rep_as_mixing_matrix
#' 
#' @export
as.matrix.mixing_matrix <- function(x) {
  mat_attrs <- c("dim", "dimnames")
  for (i in names(attributes(x))) {
    if (!i %in% mat_attrs) {
      attr(x, i) <- NULL
    }
  }
  class(x) <- "matrix"
  x
}

#' @rdname rep_as_mixing_matrix
#' 
#' @export
as.data.frame.mixing_matrix <- function(x, stringsAsFactors = FALSE, ...) {
  temp <- as.data.frame(as.matrix(x), stringsAsFactors = stringsAsFactors, ...)
  out <- cbind.data.frame(data.frame(`.ego` = rownames(temp), stringsAsFactors = stringsAsFactors),
                          `rownames<-`(temp, NULL), 
                          stringsAsFactors = stringsAsFactors)
  `rownames<-`(out, NULL)
}






