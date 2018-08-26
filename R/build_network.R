# build_network <- function(edg_attrs, vrt_attrs, net_attrs) {
#   if("name" %in% names(vrt_attrs)) {
#     names(vrt_attrs)[names(vrt_attrs) == "name"] <- "vertex.names"
#   }
#   
#   vrt_attrs
# }
# 
# vrt_attrs <- tidyUK %>% 
#   as.data.frame(stringsAsFactors = FALSE)
# 
# edg_attrs <- tidyUK %>% 
#   tidygraph::activate(edges) %>% 
#   as.data.frame(stringsAsFactors = FALSE)
# 
# net_attrs <- net_get_attrs(tidyUK)
# 
# build_network(edg_attrs, vrt_attrs, net_attrs)
# 
# enron %>% as_network()
# 
# tidy_test <- build_test_graph("ig", n = 5000) %>% 
#   tidygraph::as_tbl_graph()
# 
# tidy_test %>% as_network()
