ig <- build_test_graph("ig") %>% 
  sna_standardize_graph()

round_trip_ig <- ig %>% 
  sna_as_network() %>% 
  sna_as_igraph()

sna_identical(round_trip_ig, ig)

nw <- build_test_graph("nw") %>% 
  sna_standardize_graph()

round_trip_nw <- nw %>% 
  sna_as_igraph() %>% 
  sna_as_network()

all.equal(round_trip_nw, nw)


#### good to go
nw_directed_test <- function() {
  nw <- build_test_graph("nw") %>% 
    sna_standardize_graph()
  round_trip <- nw %>% 
    sna_as_igraph() %>% 
    sna_as_network()
  
  identical(nw, round_trip)
}
#### good to go
nw_undirected_test <- function() {
  nw <- build_test_graph("nw", direct = FALSE) %>% 
    sna_standardize_graph()
  round_trip <- nw %>% 
    sna_as_igraph() %>% 
    sna_as_network()
  
  identical(nw, round_trip)
}
#### good to go
ig_directed_test <- function() {
  ig <- build_test_graph("ig") %>% 
    sna_standardize_graph()
  round_trip <- ig %>% 
    sna_as_network() %>% 
    sna_as_igraph()
  
  identical(unclass(ig)[1:9], unclass(round_trip)[1:9])
}
#### good to go
ig_undirected_test <- function() {
  ig <- build_test_graph("ig", direct = FALSE) %>% 
    sna_standardize_graph()
  round_trip <- ig %>% 
    sna_as_network() %>% 
    sna_as_igraph()
  
  identical(unclass(ig)[1:9], unclass(round_trip)[1:9])
}
#### good to go
ig_bipartite_test <- function() {
  ig <- build_test_graph("ig", bipart = TRUE) %>%
    sna_standardize_graph()
  round_trip <- ig %>% 
    sna_as_network() %>% 
    sna_as_igraph()
  
  identical(unclass(ig)[1:9], unclass(round_trip)[1:9])
}
#### good to go

web_of_lies_network_object <- build_test_graph("nw", direct = FALSE) %>% 
  sna_standardize_graph()

round_trip_to_igraph_and_back <- web_of_lies_network_object %>% 
  sna_as_igraph() %>% 
  sna_as_network()
  
identical(web_of_lies_network_object, round_trip_to_igraph_and_back)



