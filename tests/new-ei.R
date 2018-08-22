# # ei_index <- function(x, vrt_attr, scope = c("global", "group", "vertex", "full"), drop_loops = FALSE) {
# #   scope <- match.arg(scope, c("global", "group", "vertex", "full"), several.ok = FALSE)
# #   
# #   if(drop_loops) {
# #     x <- drop_loops(x)
# #   }
# #   if(scope %in% c("global", "full")) {
# #     ei_global <- ei_index_global(x, vrt_attr)
# #     if(scope == "global") {
# #       return(ei_global)
# #     }
# #   }
# #   if(scope %in% c("group", "full")) {
# #     ei_grp <- ei_index_grp(x, vrt_attr)
# #     if(scope == "group") {
# #       return(ei_grp)
# #     }
# #   }
# #   if(scope %in% c("vertex", "full")) {
# #     ei_vrt <- ei_index_vrt(x, vrt_attr)
# #     if(scope == "vertex") {
# #       return(ei_vrt)
# #     }
# #   }
# #   
# #   out <- list(global = ei_global,
# #               by_group = ei_grp,
# #               by_vertex = ei_vrt)
# #   class(out) <- "ei_index_full"
# #   
# #   out
# # }
# 
# ei_index(samplk1, "group", scope = "vertex")
# test
# 
# 
# test_ei <- function(x, vrt_attr, iterations = 1000L, diagonal = FALSE) {
#   if(!class(x) %in% c("igraph", "network")) {
#     stop("`x` must be of class `igraph` or `network`.", call. = FALSE)
#   }
#   if(class(x) == "network") {
#     directed <- x$gal$directed
#   }
#   if(class(x) == "igraph") {
#     directed <- igraph::is_directed(x)
#   }
#   observed <- ei_index(x, vrt_attr, scope = "full")            # value test
#   
#   attr_adj_mat <- rep_attr_adj_mat(x, vrt_attr)                # matrix to permute
#   grp_cats <- unique(rownames(attr_adj_mat))
#   vrt_names <- vrt_names(x)
#   
#   global_eis <- vector("double", iterations)                   # initialize storage vectors
#   group_eis <- list(replicate(vector, "numeric", length(grp_cats)))
#   names(group_eis) <- cats
#   vertex_eis <- list(replicate(vector, "numeric", length(vrt_names)))
#   names(vertex_eis) <- vrt_names
#   
# 
#   
#   if(directed) {
#     for(i in seq_len(iterations)) {
#       permuted_matrix <- snatools:::permute_matrix(attr_adj_mat, # permute the matrix
#                                                    out_class = "attr_adj_mat")
#       if(!diagonal) {
#         diag(permuted_matrix) <- NA_integer_                     # remove the diagonal
#       }
#       global_eis[[i]] <- ei_index_global(permuted_matrix)
#       
#       
#       permuted_attr_el <- rep_attr_el(permuted_matrix)        # convert to el for fast EI
#       permuted_eis[[i]] <- ei_index_global(permuted_attr_el)  # calc EI, add to `permuted_eis`
#     }
#   } 
#   # else { # for undirected networks: remove the upper triangle
#   #   for(i in seq_len(iterations)) {
#   #     permuted_matrix <- snatools:::permute_matrix(attr_adj_mat, out_class = "attr_adj_mat")
#   #     permuted_matrix[upper.tri(permuted_matrix, diag = !diagonal)] <- NA_integer_
#   #     permuted_attr_el <- rep_attr_el(permuted_matrix)
#   #     permuted_eis[[i]] <- ei_index_global(permuted_attr_el)
#   #   }
#   # }
#   out <- list(vrt_attr = vrt_attr,
#               observed_ei = observed_ei,
#               iterations = iterations,
#               permuted_eis = permuted_eis,
#               n_greater = length(which(permuted_eis >= observed_ei)),
#               prop_greater = mean(as.numeric(permuted_eis >= observed_ei)),
#               n_lesser = length(which(permuted_eis < observed_ei)),
#               prop_lesser = mean(as.numeric(permuted_eis < observed_ei)))
#   class(out) <- "ei_index_global_permute"
# 
#   out
# }
# 
# 
# 
# 
# 
# 
# 
# ei_index_vrt.attr_adj_mat <- function(x, vrt_names) {
#   attrs <- vrt_attrs(x)[[vrt_attr]]
#   attr_mat <- x
#   rownames(attr_mat) <- vrt_names
#   mix_mat_names <- t(rowsum(t(attr_mat), group = colnames(attr_mat), na.rm = TRUE))
#   
#   mix_mat_for_external <- `rownames<-`(mix_mat_names, attrs)
#   matches <- match(rownames(mix_mat_for_external), colnames(mix_mat_for_external))
#   match_mat <- cbind(seq_along(matches), matches)
#   mix_mat_for_external[match_mat] <- NA_integer_
#   external <- rowSums(mix_mat_for_external, na.rm = TRUE)
#   names(external) <- vrt_names
# 
#   mix_mat_for_internal <- `rownames<-`(mix_mat_names, attrs)
#   mix_mat_for_internal[!is.na(mix_mat_for_external)] <- NA_integer_
#   internal <- rowSums(mix_mat_for_internal, na.rm = TRUE)
#   names(internal) <- vrt_names
#   
#   ei <- (external - internal) / (external + internal)
#   
#   out <- data.frame(name = vrt_names,
#                     external_ties = external,
#                     internal_ties = internal,
#                     ei_index = ei,
#                     stringsAsFactors = FALSE)
#   rownames(out) <- NULL
#   class(out) <- c("ei_index_vrt", "data.frame")
#   
#   out
# }
# 
# ei_index_global.attr_adj_mat <- function(x) {
#   attr_el <- rep_attr_el(x)
#   ei_index_global(attr_el)
# }
# rep_attr_adj_mat(samplk1, "group") %>% ei_index_global()
# 
# 
# 
# 
# 
