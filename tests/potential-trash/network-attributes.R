#' Extract non-structural network attribute names.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return `character` `vector` listing the names of `x`'s non-structural network 
#' attributes.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' @export
net_attr_names <- function(x) {
  UseMethod("net_attr_names")
}

#' @rdname net_attr_names
#' 
#' @examples 
#' net_attr_names(sampson_monastery)
#' 
#' @export
net_attr_names.bridge_net <- function(x) {
  names(x[["net_attrs"]])
}

#' @rdname net_attr_names
#' 
#' @examples 
#' net_attr_names(ig)
#' 
#' @importFrom igraph graph_attr_names
#' @export
net_attr_names.igraph <- function(x) {
  out <- graph_attr_names(x)
  if (is_empty(out)) {
    out <- NULL
  }
  out
}

#' @rdname net_attr_names
#' 
#' @examples 
#' net_attr_names(nw)
#' 
#' @export
net_attr_names.network <- function(x) {
  misonomers <- c("n", "directed", "hyper", "loops", "multiple", "bipartite", "mnext")
  out <- names(x[["gal"]])[!names(x[["gal"]]) %in% misonomers]
  if (is_empty(out)) {
    out <- NULL
  }
  out
}

#' @rdname net_attr_names
#' 
#' @examples
#' net_attr_names(tidy_g)
#' 
#' @export
net_attr_names.tbl_graph <- function(x) {
  net_attr_names.igraph(as_igraph.tbl_graph(x))
}


#' Extract a specific, non-strucural network attribute.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' @param net_attr `character` specifying the target network attribute.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#'
#' @export
net_get_attr <- function(x, net_attr) {
  UseMethod("net_get_attr")
}

#' @rdname net_get_attr
#' 
#' @examples 
#' sampson_monastery %>% 
#'   net_get_attr("network_name")
#' 
#' @export
net_get_attr.bridge_net <- function(x, net_attr) {
  validate_net_attr(x, net_attr)
  x[["net_attrs"]][[net_attr]]
}


#' @rdname net_get_attr
#' 
#' @examples
#' ig %>% 
#'   net_get_attr("author")
#' 
#' @seealso [igraph::graph_attr()]
#' 
#' @importFrom igraph graph_attr
#' @export
net_get_attr.igraph <- function(x, net_attr) {
  validate_net_attr(x, net_attr)
  graph_attr(x, net_attr)
}

#' @rdname net_get_attr
#' 
#' @seealso [network::get.network.attribute()]
#' 
#' @export
net_get_attr.network <- function(x, net_attr) {
  validate_net_attr(x, net_attr)
  x[["gal"]][[net_attr]]
}

#' @rdname net_get_attr
#' 
#' @examples
#' tidy_g %>% 
#'   net_get_attr("author")
#'
#' @export
net_get_attr.tbl_graph <- function(x, net_attr) {
  validate_net_attr(x, net_attr)
  net_get_attr.igraph(as_igraph.tbl_graph(x), net_attr)
}

#' Extract all non-structural network attributes as a named `list`.
#' 
#' @param x A `bridge_net`, `igraph`, `network`, or `tbl_graph`.
#' 
#' @return A named `list()` of all of `x`'s network attributes.
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' library(snatools)
#' 
#' data(sampson_monastery)
#' 
#' ig <- sampson_monastery %>% 
#'   as_igraph()
#'   
#' nw <- sampson_monastery %>% 
#'   as_network()
#'   
#' tidy_g <- ig %>% 
#'   tidygraph::as_tbl_graph()
#' 
#' @export
net_attrs_to_list <- function(x) {
  UseMethod("net_attrs_to_list")
}

#' @rdname net_attrs_to_list
#' 
#' @examples 
#' net_attrs_to_list(sampson_monastery)
#'
#' @export
net_attrs_to_list.bridge_net <- function(x) {
  x[["net_attrs"]]
}

#' @rdname net_attrs_to_list
#' 
#' @examples 
#' net_attrs_to_list(ig)
#' 
#' @importFrom igraph graph_attr
#' @export
net_attrs_to_list.igraph <- function(x) {
  out <- graph_attr(x)
  if (is_empty(out)) {
    return(NULL)
  }
  out
}

#' @rdname net_attrs_to_list
#' 
#' @examples 
#' net_attrs_to_list(nw)
#' 
#' @export
net_attrs_to_list.network <- function(x) {
  attr_names <- net_attr_names(x)
  if (is_empty(attr_names)) {
    return(NULL)
  }
  out <- lapply(attr_names, function(y) net_get_attr(x, y))
  `names<-`(out, attr_names)
}

#' @rdname net_attrs_to_list
#' 
#' @examples 
#' net_attrs_to_list(tidy_g)
#' 
#' @export
net_attrs_to_list.tbl_graph <- function(x) {
  net_attrs_to_list.igraph(as_igraph.tbl_graph(x))
}









