#' #' Build a network/graph's mixing matrix.
#' #' 
#' #' @param x An `igraph` or `network` object.
#' #' @param vrt_attr `character` representing a vertex attribute.
#' #' 
#' #' 
#' #' @export
#' #' 
#' mix_mixing_matrix <- function(x, vrt_attr, drop_loops = FALSE) {
#'   UseMethod("mix_mixing_matrix")
#' }
#' 
#' #' @rdname mix_mixing_matrix
#' #' 
#' #' @export
#' #' 
#' mix_mixing_matrix.igraph <- function(x, vrt_attr, drop_loops = FALSE) {
#'   if(!is.character(vrt_attr)) {
#'     stop("`vrt_attr` must be a `character`.", call. = FALSE)
#'   }
#'   if(!vrt_attr %in% igraph::vertex_attr_names(x)) {
#'     stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
#'   }
#'   if(drop_loops) {
#'     x <- igraph::simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
#'   }
#'   attrs <- igraph::vertex_attr(x, vrt_attr)
#'   cats <- sort(unique(attrs))
#'   el <- igraph::as_edgelist(x, names = FALSE)
#'   el_list <- list(el[, 1], el[, 2])
#'   init <- lapply(el_list, function(x) {
#'     factor(attrs[x], levels = cats)
#'   })
#'   # table(from = init[[1]], to = init[[2]])
#'   from <- factor(attrs[el[, 1]], levels = cats)
#'   to <- factor(attrs[el[, 2]], levels = cats)
#'   out <- table(from, to)
#'   class(out) <- "matrix"
#'   
#'   out
#' }
#' 
#' #' @rdname mix_mixing_matrix
#' #' 
#' #' @export
#' #' 
#' mix_mixing_matrix.network <- function(x, vrt_attr, drop_loops = FALSE) {
#'   if(!is.character(vrt_attr)) {
#'     stop("`vrt_attr` must be a `character`.", call. = FALSE)
#'   }
#'   if(!vrt_attr %in% unlist(lapply(x$val, names))) {
#'     stop("`vrt_attr` is not a vertex attribute in `x`", call. = FALSE)
#'   }
#'   if(drop_loops) {
#'     network::set.network.attribute(x, "loops", value = FALSE)
#'   }
#'   attrs <- network::get.vertex.attribute(x, vrt_attr)
#'   cats <- sort(unique(attrs))
#'   el <- network::as.matrix.network.edgelist(x)
#'   if(nrow(el) == 0L) {
#'     message("`x` does not have any edges.")
#'   }
#'   from <- factor(attrs[el[, 1]], levels = cats)
#'   to <- factor(attrs[el[, 2]], levels = cats)
#'   out <- table(from, to)
#'   class(out) <- "matrix"
#'   
#'   out
#' }
