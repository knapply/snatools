# #' Read UCINET's .dl files
# #' 
# #' @param path Path to file or URL.
# #' 
# #' @return A `net_primitive` object.
# #' 
# #' @examples 
# #' library(snatools)
# #' 
# #' "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/sampson.dat" %>%
# #'   read_ucinet()
# #'   
# #' "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/davis.dat" %>% 
# #'   read_ucinet() %>% 
# #'   as_igraph()
# #'   
# #' "http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/newfrat.dat" %>% 
# #'   read_ucinet() %>% 
# #'   as_network()
# #'
# # @importFrom dplyr bind_rows distinct everything mutate select
# # @importFrom igraph as_data_frame graph_from_adjacency_matrix graph_from_incidence_matrix
# # @importFrom igraph graph_from_data_frame is_bipartite is_directed
# # @importFrom igraph is.igraph is.loop is.multiple
# # @importFrom magrittr %>%
# # @importFrom purrr imap map map_df
# # @importFrom tibble as_tibble tibble
# read_ucinet <- function(path, directed = NULL) {
#   raw <- readLines(path)
#   file_type <- raw[[1]]
#   if(txt_to_lower(file_type) != "dl"){
#     stop("`read_ucinet()` expects line 1 to be 'DL' or 'dl' (Data Language)", 
#          call. = FALSE)
#   }
#   node_count <- as.integer(txt_extract(raw[[2]], "(?<=^N=)\\d+"))
#   if (!length(node_count)) {
#     two_mode <- TRUE
#     row_count <- as.integer(txt_extract(raw[[2]], "(?<=^NR=)\\d+"))
#     col_count <- as.integer(txt_extract(raw[[2]], "(?<=NC=)\\d+"))
#   } else {
#     two_mode <- FALSE
#   }
#   matrix_count <- as.integer(txt_extract(raw[[2]], "(?<=\\sNM=)\\d+$"))
#   if(length(matrix_count) == 0L) {
#     matrix_count <- 1L
#   }
#   file_format <- txt_extract(txt_subset(raw, "FORMAT\\s=\\s"), "(?<=\\s=\\s).*$")
#   n_row <- as.integer(txt_extract(raw[[2]], "(?<=^NR=)\\d+"))
#   if (length(n_row) == 0) {
#     n_row <- node_count
#   }
#   n_col <- as.integer(txt_extract(raw[[2]], "(?<=\\sNC=)\\d+$"))
#   if(length(n_col) == 0) {
#     n_col <- node_count
#   }
#   
#   attr_lines <- txt_which(raw, ":$")
#   names(attr_lines) <- txt_remove(txt_subset(raw, ":$"), ":")
#   
#   data_raw <- raw[(attr_lines["DATA"] + 1):length(raw)]
#   data_trimmed <- txt_squish(txt_trim(data_raw))
#   data_list <- lapply(txt_split(data_trimmed, "\\s+"), as.integer)
# 
#   if (matrix_count == 1L) {
#     data_flat <- unlist(data_list)
#     mats <- matrix(data_flat, nrow = n_row, ncol = n_col, byrow = TRUE)
#   }
#   if (matrix_count > 1L) {
#     split_list <- split(data_list, rep(seq_len(matrix_count), each = n_row))
#     flat_matrices <- lapply(split_list, unlist)
#     mats <- lapply(flat_matrices, matrix, nrow = n_row, ncol = n_col, byrow = TRUE)
#   }
#   if (!is.na(attr_lines["ROW LABELS"])) {
#     row_labels <- raw[(attr_lines["ROW LABELS"] + 1):(attr_lines["COLUMN LABELS"] - 1)]
#     if (is.list(mats)) {
#       mats <- lapply(mats, `rownames<-`, row_labels)
#     } else {
#       rownames(mats) <- row_labels
#     }
#   } else {
#     row_labels <- NULL
#   }
#   if (!is.na(attr_lines["COLUMN LABELS"])) {
#     if(is.list(mats)) {
#       col_labels <- raw[(attr_lines["COLUMN LABELS"] + 1):(attr_lines["LEVEL LABELS"] - 1)]
#       mats <- lapply(mats, `colnames<-`, col_labels)
#     } else {
#       col_labels <- raw[(attr_lines["COLUMN LABELS"] + 1):(attr_lines["DATA"] - 1)]
#       colnames(mats) <- col_labels
#     }
#   } else {
#     col_labels <- NULL
#   }
#   if (!is.na(attr_lines["LEVEL LABELS"])) {
#     lev_labels <- raw[(attr_lines["LEVEL LABELS"] + 1):(attr_lines["DATA"] - 1)]
#     names(mats) <- lev_labels
#   } else {
#     lev_labels <- NULL
#   }
#   weighted <- any(vapply(mats, function(x) length(x[!x %in% c(0, 1)]) > 0L, logical(1)))
#   if (is.null(directed)) {
#     if (!two_mode) {
#       if (is.list(mats)) {
#         directed <- any(vapply(mats, function(x) !isSymmetric(x), logical(1)))
#       } 
#     } else {
#       directed <- FALSE
#     }
#   }
#   if (directed) {
#     directed_chr <- "directed"
#   } else {
#     directed_chr <- "undirected"
#   }
#   if (!two_mode) {
#     if (is.list(mats)) {
#       all_gs <- purrr::map(mats, 
#                            ~ igraph::graph_from_adjacency_matrix(.x, mode = directed_chr, 
#                                                                  weighted = TRUE))
#     } else {
#       all_gs <- igraph::graph_from_adjacency_matrix(mats, mode = directed_chr, 
#                                                     weighted = TRUE)
#     }
#   } else {
#     if (is.list(mats)) {
#       all_gs <- purrr::map(mats, 
#                            ~ igraph::graph_from_incidence_matrix(.x, directed = directed))
#       all_gs <- map(all_gs, bip_swap_modes)
#     } else {
#       all_gs <- igraph::graph_from_incidence_matrix(mats, directed = directed)
#       all_gs <- bip_swap_modes(all_gs)
#     }
#   }
#   if (!igraph::is.igraph(all_gs)) {
#     init_edges <- purrr::imap(all_gs, ~ igraph::as_data_frame(.x, what = "edges") %>% 
#                                 tibble::as_tibble() %>% 
#                                 dplyr::mutate(level = .y)
#                       )
#     init_edges <- dplyr::bind_rows(init_edges)
#     init_vertices <- purrr::map_df(all_gs, igraph::as_data_frame, what = "vertices")
#   } else {
#     init_edges <- igraph::as_data_frame(all_gs, what = "edges")
#     init_vertices <- igraph::as_data_frame(all_gs, what = "vertices")
#   }
#   init_edges <- tibble::as_tibble(init_edges)
#   init_vertices <- tibble::as_tibble(init_vertices)
#   if (!ncol(init_vertices)) {
#     init_vertices <- tibble::tibble(name = sort(unique(c(init_edges$from, init_edges$to))))
#   }
#   init_vertices <- dplyr::select(init_vertices, name, everything())
#   init_vertices <- dplyr::distinct(init_vertices)
# 
#   out <- igraph::graph_from_data_frame(init_edges, directed, vertices = init_vertices)
#   if ("weight" %in% igraph::edge_attr_names(out)) {
#     if (all(igraph::edge_attr(out, "weight") == 1L)) {
#       out <- igraph::delete_edge_attr(out, "weight")
#     }
#   }
#   as_bridge_net(out)
# }