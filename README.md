
<!-- README.Rmd generates README.md. -->
snatools
========

<!-- <img src="http://res.cloudinary.com/syknapptic/image/upload/v1516468904/logo_rd5ifq.png" align="right" height="160px" width="240px" /> -->
[![](https://img.shields.io/badge/devel%20version-0.1-red.svg)](https://github.com/knapply/snatools)

<br>

``` r
library(snatools)

network_obj <- snatools:::build_test_graph("nw") %>% 
  sna_clean_graph()

identical(
  network_obj,
  sna_as_network(sna_as_igraph(network_obj))
  )
#> [1] TRUE
```

{`testthat`}!
=============

``` r
devtools::test()
#> Loading snatools
#> Testing snatools
#> v | OK F W S | Context
#> 
/ |  0       | Round trip conversion: undirected graphs
- |  1       | Round trip conversion: undirected graphs
\ |  2       | Round trip conversion: undirected graphs
v |  2       | Round trip conversion: undirected graphs [0.1 s]
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
#> == Results =====================================================================
#> Duration: 0.3 s
#> 
#> OK:       12
#> Failed:   0
#> Warnings: 0
#> Skipped:  0
```
