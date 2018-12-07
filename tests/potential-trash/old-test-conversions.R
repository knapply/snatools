# context("conversions")
# 
# converts_perfectly <- function(n_nodes, directed, bipartite) {
#   ig <- build_test_graph("ig", n_nodes, directed = directed, bipartite = bipartite)
#   ig_net1 <- as_sna_net(ig)
#   ig_net2 <- as_sna_net(as_network(ig))
#   
#   nw <- build_test_graph("nw", n_nodes, directed = directed, bipartite = bipartite)
#   nw_net1 <- as_sna_net(nw)
#   nw_net2 <- as_sna_net(as_igraph(nw))
#   
#   all(c(identical(ig_net1, nw_net1), 
#         identical(ig_net2, nw_net2)))
# }
# 
# test_conversions <- function() {
#   arg_combos <- data.frame(n_nodes = c(0, sample(seq(1, 50), 10)),
#                            directed = c(TRUE, FALSE, rep(NA, 9)),
#                            bipartite = c(TRUE, FALSE, rep(NA, 9)))
#   arg_combos <- expand.grid(arg_combos, stringsAsFactors = FALSE)
#   arg_combos <- na.omit(arg_combos)
#   arg_combos <- lapply(arg_combos, unlist)
#   
#   results <- vector("logical", length(arg_combos[[1]]))
#   for (i in seq_len(length(arg_combos[[1]]))) {
#     res <- tryCatch(purrr::pmap_lgl(purrr::map(arg_combos, i), converts_perfectly),
#                     error = function(e) {
#                       message(i, " threw an error. Returning `arg_combos[[", i, "]]`")
#                       })
#     if (!is.logical(res)) {
#       return(purrr::map(arg_combos, i))
#     }
#     if (!res) message(i, " failed")
#     results[[i]] <- res
#   }
#   all(results)
# }
# 
# test_that("all conversions work perfectly", {
#   expect_true(test_conversions())
#   })
