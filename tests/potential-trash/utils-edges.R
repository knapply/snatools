#' #' @export
#' drop_loops <- function(x) {
#'   UseMethod("drop_loops")
#' }
#' 
#' #' @importFrom igraph simplify
#' #' @export
#' drop_loops.igraph <- function(x) {
#'   simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
#' }
#' 
#' #' @importFrom network set.network.attribute
#' #' @export
#' drop_loops.network <- function(x) {
#'   out <- set.network.attribute(x, "loops", value = FALSE)
#'   out
#' }