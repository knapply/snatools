#' Is an Object Directed?
#'
#' @template param-x
#'
#' @return `<logical>`
#'
#' @template author-bk
#'
#' @seealso [igraph::is_directed()], [network::is.directed()]
#'
#' @export
net_is_directed <- function(x, ...) {
  UseMethod("net_is_directed")
}

#' @rdname net_is_directed
#'
#' @examples
#' ig <- example_igraph()
#' net_is_directed(ig)
#'
#' @importFrom igraph is_directed
#'
#' @export
net_is_directed.igraph <- function(x, ...) {
  is_directed(x)
}

#' @rdname net_is_directed
#'
#' @examples
#' nw <- example_network()
#' net_is_directed(nw)
#'
#' @export
net_is_directed.network <- function(x, ...) {
  x[["gal"]][["directed"]]
}
