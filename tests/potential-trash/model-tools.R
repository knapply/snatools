# x <- build_test_graph("ig")
# 
# model.matrix.default(~ node_chr, vrt_get_attr_df(build_test_graph("ig")))
# 
# attrs_as_dummies <- function(x, vertices_or_edges = c("vertices", "edges"),
#                              target_attrs = NULL) {
#   vertices_or_edges <- match.arg(vertices_or_edges, c("vertices", "edges"))
#   if (vertices_or_edges == "vertices") {
#     attr_df <- vrt_get_attr_df(x, sna_vertices_df = FALSE)
#     if (is.null(target_attrs)) {
#       target_attrs <- vrt_attr_names(x)[!vrt_attr_names(x) == get_vrt_names_attr(x)]
#       for (attr in target_attrs) {
#         x <- delete_vertex_attr(x, attr)
#       }
#       preserve_df <- vrt_get_attr_df(x)
#     } else {
#       for (attr in target_attrs) {
#         x <- delete_vertex_attr(x, attr)
#       }
#     }
#   } else if (vertices_or_edges == "edges") {
#     attr_df <- edg_get_attr_df(x, leave_raw = TRUE, include_dyad = FALSE)
#     if (is.null(target_attrs)) {
#       for (attr in edg_attr_names(x)) {
#         x <- delete_edge_attr(x, attr)
#       }
#     } else {
#       for (attr in target_attrs) {
#         x <- delete_edge_attr(x, attr)
#       }
#     }
#     preserve_df <- edg_get_attr_df(x, include_dyad = FALSE, leave_raw = TRUE)
#   }
#   if (nrow(preserve_df) == 0L) {
#     preserve_df <- NULL
#   }
#   if (is.null(target_attrs)) {
#     form <- as.formula("~ .")
#   } else {
#     form <- as.formula(paste0("~", target_attrs, sep = ""))
#   }
#   new_attrs_df <- as.data.frame.matrix(model.matrix.default(form, data = attr_df),
#                                        stringsAsFactors = FALSE)
#   new_attrs_df[["(Intercept)"]] <- NULL
#   
#   if (!is.null(preserve_df)) {
#     new_attrs_df <- cbind.data.frame(preserve_df, new_attrs_df, stringsAsFactors = FALSE)
#   }
#   if (vertices_or_edges == "vertices") {
#     vertex_attr(x) <- new_attrs_df
#   } else if (vertices_or_edges == "edges") {
#     edge_attr(x) <- new_attrs_df
#   }
#   x
# }
# attrs_as_dummies(build_test_graph("ig"), vertices_or_edges = "edges") 
# %>%
#   vrt_get_attr_df()
# 
# 
# 
# 
# 
