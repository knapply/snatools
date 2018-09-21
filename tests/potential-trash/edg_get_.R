#' #' Extract Edge Attributes
#' #' 
#' #' Extract edge attributes in a variety of formats consistent across
#' #' `igraph` and `network` objects.
#' #' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @param x A graph object.
#' #' @param ... Arguments passed on to other methods. See below.
#' #' @param include_el `logical`. Whether to include edge list columns indicating which
#' #' vertices are part of each edge's dyad.
#' #' @param ignore_na `logical`. Whether to ignore the `"na"` attribute of 
#' #' [`network::network`] objects. \cr
#' #' Default: `TRUE`
#' #' @param edg_attr `character`. Name of target attribute.
#' #' Whether to attempt calling [`tibble::as_tibble()`] in `edg_get_attrs_df()`. \cr
#' #' Default: `TRUE`
#' #' 
#' #' @return A named `list`, `vector`, or [`tibble::tibble`]. See Details.
#' #' 
#' #' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' #' 
#' #' @examples
#' #' library(snatools)
#' #' 
#' #' data("enron", package = "igraphdata")
#' #' (ig <- enron)
#' #' 
#' #' data("sampson", package = "ergm")
#' #' (nw <- samplike)
#' #' 
#' 
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @details 
#' #' * `edg_get_attrs` extract all edge attributes as a named `tibble::tibble`.
#' #' 
#' #' @seealso 
#' #' [`igraph::as_data_frame()`]
#' #' 
#' #' @examples 
#' #' edg_get_attrs(ig, include_el = TRUE)
#' #' edg_get_attrs(nw, include_el = TRUE)
#' #' 
#' #' @export
#' edg_get_attrs <- function(x, ...) {
#'   UseMethod("edg_get_attrs")
#' }
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @importFrom igraph as_data_frame
#' #' @importFrom tibble as_tibble
#' #' @export
#' edg_get_attrs.igraph <- function(x, include_el = FALSE) {
#'   out <- as_data_frame(x)
#'   if (!include_el) {
#'     out$from <- NULL
#'     out$to <- NULL
#'   }
#'   as_tibble(out)
#' }
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @importFrom dplyr bind_cols
#' #' @importFrom purrr map_df
#' #' @importFrom tibble as_tibble
#' #' @export
#' edg_get_attrs.network <- function(x, include_el = FALSE, ignore_na = TRUE) {
#'   out <- map_df(x$mel, "atl")
#'   if (include_el) {
#'     el <- rep_edgelist(x, use_names = TRUE)
#'     el <- as_tibble(el)
#'     out <- bind_cols(el, out)
#'   }
#'   if (ignore_na) {
#'     out$na <- NULL
#'   }
#'   out
#' }
#' 
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @details 
#' #' * `edg_get_attr()` extracts the single edge attribute specified by `edg_attr` as a `vector`.
#' #' 
#' #' @seealso 
#' #' [`igraph::edge_attr()`], [`network::get.edge.attribute()`]
#' #' 
#' #' @examples 
#' #' edg_get_attr(ig, "Time") %>% head(20)
#' #' edg_get_attr(nw, "nominations") %>% head(20)
#' #' 
#' #' @export
#' edg_get_attr <- function(x, edg_attr, ...) {
#'   UseMethod("edg_get_attr")
#' }
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @importFrom igraph edge_attr
#' #' @export
#' edg_get_attr.igraph <- function(x, edg_attr, ...) {
#'   edg_get_attrs(x, include_el = FALSE)[[edg_attr]]
#' }
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @importFrom network get.edge.attribute
#' #' @export
#' edg_get_attr.network <- function(x, edg_attr, ignore_na = TRUE, ...) {
#'   edg_get_attrs(x, include_el = FALSE, ignore_na = FALSE)[[edg_attr]]
#' }
#' 
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @details 
#' #' * `edg_get_attr_names()` extract the names of vertex attributes as a `character` `vector`.
#' #'   + `edg_get_attr_names()` differs from `network::list.edge.attributes()`, which
#' #'   sorts attribute names alphabetically.
#' #' 
#' #' @seealso 
#' #' [`igraph::edge_attr_names()`], [`network::list.edge.attributes()`]
#' #' 
#' #' @export
#' edg_get_attr_names <- function(x, ...) {
#'   UseMethod("edg_get_attr_names")
#' }
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @importFrom igraph edge_attr_names
#' #' @export
#' edg_get_attr_names.igraph <- function(x) {
#'   out <- edge_attr_names(x)
#'   if(!length(out)) {
#'     return(NULL)
#'   }
#'   out
#' }
#' 
#' #' @rdname extract-edge-attributes
#' #' 
#' #' @export
#' edg_get_attr_names.network <- function(x, ignore_na = TRUE) {
#'   out <- lapply(x$mel, `[[`, "atl")
#'   out <- unique(unlist(lapply(out, names)))
#'   if(!length(out)) {
#'     return(NULL)
#'   }
#'   if(!ignore_na) {
#'     return(out)
#'   }
#'   out[!out == "na"]
#' }
#' 
#' 
#' # vrt_named <- function(x) {
#' #   if (!class(x) == "sna_net") {
#' #     x <- as_sna_net(x, edg_attrs = FALSE, net_attrs = FALSE)
#' #   }
#' #   if (is.null(x[["vertices"]])) {
#' #     return(FALSE)
#' #   }
#' #   if (!is.integer(x[["vertices"]][[".name"]])) {
#' #     return(FALSE)
#' #   }
#' #   if (all.equal(x[["vertices"]][[".name"]], seq_len(nrow(x[["vertices"]])))) {
#' #     return(FALSE)
#' #   }
#' #   if (all(x[["vertices"]][[".name"]] %in% seq_len(nrow(texmp[["vertices"]])))) {
#' #     return(FALSE)
#' #   }
#' #   TRUE
#' # }
#' # test_ig2 <- build_test_graph("ig", 500)
#' # 
#' # test_nw <- as_network(test_ig2)
#' # 
#' # library(tidygraph)
#' # library(ergm)
#' # 
#' # 
#' # 
#' # # nw <- 
#' # play_barabasi_albert(30, 3) %>% 
#' #   mutate(age = sample(seq_len(30), nrow(.N()), replace = TRUE)) %E>%
#' #   mutate(weight = sample(runif(nrow(.E())))) %>% 
#' #   as_network() %>% 
#' #   search.ergmTerms(net = .)
#' #   as_network()
#' #   
#' # ergm(nw ~ edges + nodematch("age") + isolates) %>% 
#' #   summary()
#' # 
#' # 
#' # ergm(test_nw ~ edges + nodematch("color"),
#' #            control = control.ergm(MCMC.samplesize = 1000L))
#' # 
#' # res <- microbenchmark::microbenchmark(
#' #   as_sna_net(test_ig),
#' #   as_sna_net(test_nw),
#' #   times = 10
#' # )
#' # 
#' # microbenchmark::autoplot.microbenchmark(res)
#' # 
#' # 
#' # 
#' # n_edges <- 10000
#' # el <- matrix(vector("double", n_edges * 2L), ncol = 2L,
#' #              dimnames = list(NULL, c("source", "target")))
#' # el[, "source"] <- seq_len(1000)
#' # el[, "target"] <- sample(el[, "source"])
#' # 
#' # igraph::graph_from_edgelist(el)
#' # 
#' # test_nw <- build_test_graph("nw", 500)
#' # 
#' # test_nw %>% as_igraph()
#' # 
#' # microbenchmark::autoplot.microbenchmark(res)
#' # 
#' # net_is_named(test_ig_no_vrt_attrs)
#' # 
#' # edg_get_attrs2 <- function(x, use_names = TRUE) {
#' #   if (!use_names) {
#' #     return(as_sna_net(x, edg_attrs = FALSE, vrt_attrs = FALSE, net_attrs = FALSE))
#' #   }
#' #   if (!igraph::is_named(x)) {
#' #     return(as_sna_net(x, edg_attrs = FALSE, vrt_attrs = FALSE, net_attrs = FALSE))
#' #   }
#' #   temp <- as_sna_net(x)
#' #   out <- temp[["edges"]]
#' #   if (use_names && !is.null(temp[["vertices"]]) && !is.integer(temp[["vertices"]][[".name"]])) {
#' #     out[, ".ego"] <- temp[["vertices"]][[".name"]][out[, ".ego"]]
#' #     out[, ".alter"] <- temp[["vertices"]][[".name"]][out[, ".alter"]]
#' #   }
#' #   if (requireNamespace("tibble")) {
#' #     out <- tibble::as_tibble(out)
#' #   }
#' #   out
#' # }
#' # 
#' # test_ig %>% as_sna_net()
#' # 
#' # ####
#' # 
#' # test_ig <- igraph::random.graph.game(10000, 0.4)
#' # 
#' # test_ig %>% igraph::ecount() %>% scales::comma()
#' # test_ig %>% igraph::vcount() %>% scales::comma()
#' # 
#' # res <- microbenchmark::microbenchmark(
#' #   edg_get_attrs(test_ig, include_el = TRUE),
#' #   edg_get_attrs2(test_ig), times = 10
#' # )
#' # 
#' # microbenchmark::autoplot.microbenchmark(res)
#' 
#' 
