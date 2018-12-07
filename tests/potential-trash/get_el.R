#' get_el <- function(x, vrt_attr) {
#'   UseMethod("get_el")
#' }
#' 
#' get_el.network <- function(x, vrt_attr = NULL) {
#'   cbind(unlist(lapply(x[["mel"]], `[[`, "inl")),
#'         unlist(lapply(x[["mel"]], `[[`, "outl")))
#' }
#' 
#' #' @importFrom igraph as_edgelist
#' get_el.igraph <- function(x, vrt_attr = NULL) {
#'   igraph::as_edgelist(x, names = FALSE)
#' }
#' 
#' # rep_as_edgelist <- function(x, use_names = TRUE, vrt_attr = NULL,
#' #                                     leave_raw = FALSE) {
#' #   validate_graph(x)
#' #   if (!is.null(vrt_attr)) {
#' #     fill <- vrt_get_attr(x, vrt_attr)
#' #     el_type <- "vrt_attrs"
#' #     el_subtype <- vrt_attr
#' #   } else {
#' #     if (use_names) {
#' #       if (is_valid_vrt_attr(x, get_vrt_names_attr(x))) {
#' #         fill <- vrt_get_names(x)
#' #         el_type <- "vrt_names"
#' #         el_subtype <- NULL
#' #       } else {
#' #         fill <- NULL
#' #         el_type <- "vrt_indices"
#' #         el_subtype <- NULL
#' #       }
#' #     }
#' #   }
#' #   out <- get_el(x)
#' #   if (!is.null(fill)) {
#' #     out <- matrix(fill[out], ncol = 2L)
#' #   }
#' #   if (leave_raw) {
#' #     return(out)
#' #   }
#' #   set_edgelist_class(out, graph = x, el_type = el_type, el_subtype = el_subtype)
#' # }
#' 
#' # set_edgelist_class <- function(x, graph, el_type, el_subtype) {
#' #   colnames(x) <- c(".ego", ".alter")
#' #   attr(x, "el_type") <- el_type
#' #   attr(x, "el_subtype") <- el_subtype %||% NA
#' #   x <- set_metadata_attr(x, graph)
#' #   class(x) <- c("edgelist", "matrix")
#' #   x
#' # }
#' 

#' 
#' samplike %>%
#'   as.matrix() %>%
#'   el_from_adj_mat()
#' 
#' vrt_get_attr()
#' 
#' 
#' # samplike %>%
#'   # as_igraph() %>% 
#'   # rep_as_edgelist.default(vrt_attr = "group")
#' # x <- samplike
