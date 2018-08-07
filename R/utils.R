# sna_identical <- function(x, y) {
#   if(all("igraph" %in% class(x) && "igraph" %in% class(y))) {
#     return(identical(unclass(x)[1:9], unclass(y)[1:9]))
#   }
#   identical(x, y)
# }
