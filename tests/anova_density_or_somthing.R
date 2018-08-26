# library(snatools)
# library(ggplot2)
# 
# expanded_ei <- function(x, vrt_attr, iterations = 1000, diagonal = FALSE) {
#   if(!class(x) %in% c("igraph", "network")) {
#     stop("`x` must be of class `igraph` or `network`.", call. = FALSE)
#   }
#   observed_ei <- mix_ei_index(x, vrt_attr)
# 
#   attr_adj_mat <- rep_as_attr_adj_mat(x, vrt_attr)
# 
#   permuted_eis <- vector("numeric", iterations)
# 
#   for(i in seq_len(iterations)) {
#     permuted_matrix <- snatools:::permute_matrix(attr_adj_mat, out_class = "attr_adj_mat")
#     if(!diagonal) {
#       diag(permuted_matrix) <- NA_integer_
#     }
#     permuted_attr_el <- rep_as_attr_el(permuted_matrix)
#     permuted_eis[[i]] <- mix_ei_index(permuted_attr_el)
#   }
#   out <- list(vrt_attr = vrt_attr,
#               observed_ei = observed_ei,
#               iterations = iterations,
#               permuted_eis = permuted_eis,
#               n_greater = length(which(permuted_eis >= observed_ei)),
#               prop_greater = mean(as.numeric(permuted_eis >= observed_ei)),
#               n_lesser = length(which(permuted_eis < observed_ei)),
#               prop_lesser = mean(as.numeric(permuted_eis < observed_ei)))
#   class(out) <- "expanded_ei"
# 
#   out
# }
# 
# plot_it <- function(x) { # just plots
#   data.frame(perm_ei = x$permuted_eis) %>%
#     ggplot() +
#     stat_density(aes(perm_ei), fill = "orange", alpha = 0.5) +
#     geom_vline(aes(xintercept = x$observed_ei, color = "Observed E-I Index")) +
#     scale_x_continuous(limits = c(-1, 1)) +
#     guides(color = guide_legend(NULL)) +
#     theme_minimal(12, "serif") +
#     theme(plot.title = element_text("mono"), plot.subtitle = element_text("mono"),
#           plot.caption = element_text("mono"), legend.position = "top") +
#     labs(x = paste("E-I Indices of Permuted Matrices"), y = "Frequency",
#          caption = paste("iterations =", x$iterations))
# }
# 
# # data =========================================================================================
# data("samplk", package = "ergm")
# niter <- 50000
# 
# # example 1 ====================================================================================
# cloisterville_attr <- expanded_ei(samplk1, "cloisterville", iterations = niter)
# 
# # results 1 ====================================================================================
# cloisterville_attr[!names(cloisterville_attr) == "permuted_eis"] %>% cbind()
# 
# plot_it(cloisterville_attr)
# 
# # example 2 ====================================================================================
# group_attr <- expanded_ei(samplk1, "group", iterations = niter)
# 
# # results 2 ====================================================================================
# group_attr[!names(group_attr) == "permuted_eis"] %>% cbind()
# 
# plot_it(group_attr)
# 
# # reprex::reprex()
