clean_network_metadata <- function(x) {
  if(class(x) != "network") {
    stop("`fill_network_metadata()` is only applicable to `network` objects.")
  }
  if(is.null(x$gal$bipartite)) {
    network::set.network.attribute(x, "bipartite", network::is.bipartite(x))
  }
  if(is.null(x$gal$loops)) {
    network::set.network.attribute(x, "loops", network::has.loops(x))
  }
  if(is.null(x$gal$hyper)) {
    network::set.network.attribute(x, "hyper", network::is.hyper(x))
  }
  if(is.null(x$gal$multiple)) {
    if(is.null(network::is.multiplex(x))) {
      network::set.network.attribute(x, "multiple", FALSE)
    } else {
    network::set.network.attribute(x, "multiple", network::is.multiplex(x))
    }
  }
  net_attrs <- net_get_attrs(x, drop_metadata = FALSE)
  net_attrs <- net_attrs[order(names(net_attrs))]
  for(i in names(net_attrs)) {
    network::delete.network.attribute(x, i)
    x$gal[[i]] <- NULL
  }
  for(i in seq_along(net_attrs)) {
    network::set.network.attribute(x, names(net_attrs)[[i]], net_attrs[[i]])
  }
  x
}

#' @export
#' 
drop_loops <- function(x) {
  UseMethod("drop_loops")
}

#' @importFrom igraph simplify
#' @export
drop_loops.igraph <- function(x) {
  simplify(x, remove.multiple = FALSE, remove.loops = TRUE)
}

#' @importFrom network set.network.attribute
#' @export
drop_loops.network <- function(x) {
  out <- set.network.attribute(x, "loops", value = FALSE)
  out
}

#' @export
has_loops <- function(x) {
  UseMethod("has_loops")
}

#' @importFrom igraph is.loop
#' @export
has_loops.igraph <- function(x) {
  any(is.loop(x))
}

#' @export
has_loops.network <- function(x) {
  el <- rep_edgelist(x)
  any(el[, 1] == el[, 2])
}

