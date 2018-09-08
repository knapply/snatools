#' @importFrom rlang is_named
sort_list_by_name <- function(x) {
  if(is_named(x)) {
    return(x[order(names(x))])
  }
  x
}

clear_metadata <- function(x) {
  metadata <- c("n", "directed", "hyper", "loops", "multiple", "bipartite", "mnext")
  out <- x[!names(x) %in% metadata]
  sort_list_by_name(out)
}

dissect_graph <- function(x) {
  net_attrs = net_get_attrs(x)
  if (length(unlist(net_attrs)) == 0L | all(is.na(unlist(net_attrs)))) {
    net_attrs <- NULL
  }
  vrt_attrs = vrt_get_attrs(x)
  if ("is_actor" %in% vrt_get_attr_names(x)) {
    vrt_attrs$type <- vrt_attrs$is_actor
    vrt_attrs$is_actor <- NULL
  }
  edg_attrs = edg_get_attrs(x)
  el <- rep_edgelist(x)
  
  if(class(x) == "network") {
    vrt_attrs$name <- vrt_attrs$vertex.names
    vrt_attrs$vertex.names <- NULL
    net_attrs <- clear_metadata(net_attrs)
  }
  out <- list(net_attrs = sort_list_by_name(net_attrs),
              vrt_attrs = sort_list_by_name(vrt_attrs),
              edg_attrs = sort_list_by_name(edg_attrs),
              el = el)
  out
}

  

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

