
<!-- README.Rmd generates README.md. -->
snatools <a href="man/figures/logo.png"> <img src="man/figures/logo.png" align="right" height="40%" width="40%" href="man/figures/logo.png"/> </a>
==================================================================================================================================================

[![](https://img.shields.io/badge/devel%20version-0.0.0.9-red.svg)](https://github.com/knapply/snatools)

<!-- <br><br><br><br><br><br><br><br><br><br><br><br><br> -->
``` r
library(snatools)

network_obj <- snatools:::build_test_graph("nw") %>% 
  clean_graph()

igraph_obj <- snatools:::build_test_graph("ig") %>% 
  clean_graph()

network_obj %==% as_network(as_igraph(network_obj)) # identical?
#> [1] TRUE

igraph_obj %==% as_igraph(as_network(igraph_obj)) # identical?
#> [1] TRUE
```

Definitions
===========

-   graph
    -   intentionally generic reference to graph/network objects of *any class*
-   `igraph`
    -   objects of class `igraph`
-   `network`
    -   objects of class `network`

Test Results
============

``` r
devtools::test()
#> Loading snatools
#> Testing snatools
#> v | OK F W S | Context
#> 
/ |  0       | Build edge lists: igraph
- |  1       | Build edge lists: igraph
\ |  2       | Build edge lists: igraph
| |  3       | Build edge lists: igraph
/ |  4       | Build edge lists: igraph
v |  4       | Build edge lists: igraph
#> 
/ |  0       | Build edge lists: network
- |  1       | Build edge lists: network
\ |  2       | Build edge lists: network
v |  2       | Build edge lists: network
#> 
/ |  0       | Round trip conversion: undirected graphs
- |  1       | Round trip conversion: undirected graphs
\ |  2       | Round trip conversion: undirected graphs
v |  2       | Round trip conversion: undirected graphs
#> 
/ |  0       | Round trip conversion: directed graphs
- |  1       | Round trip conversion: directed graphs
\ |  2       | Round trip conversion: directed graphs
v |  2       | Round trip conversion: directed graphs
#> 
/ |  0       | Round trip conversion: bipartite graphs
- |  1       | Round trip conversion: bipartite graphs
\ |  2       | Round trip conversion: bipartite graphs
v |  2       | Round trip conversion: bipartite graphs
#> 
/ |  0       | E-I Index: directed
- |  1       | E-I Index: directed
\ |  2       | E-I Index: directed
v |  2       | E-I Index: directed
#> 
/ |  0       | E-I Index: undirected
- |  1       | E-I Index: undirected
\ |  2       | E-I Index: undirected
v |  2       | E-I Index: undirected
#> 
#> == Results =====================================================================
#> Duration: 0.2 s
#> 
#> OK:       16
#> Failed:   0
#> Warnings: 0
#> Skipped:  0
#> 
#> Your tests are striking!
```
