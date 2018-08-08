
<!-- README.Rmd generates README.md. -->
snatools
========

<!-- <img src="http://res.cloudinary.com/syknapptic/image/upload/v1516468904/logo_rd5ifq.png" align="right" height="160px" width="240px" /> -->
[![](https://img.shields.io/badge/devel%20version-0.1-red.svg)](https://github.com/knapply/snatools)

<br>

``` r
network_obj <- snatools:::build_test_graph("nw") %>% 
  snatools:::sna_standardize_graph.network()

identical(
  snatools:::sna_as_network.igraph(snatools:::sna_as_igraph.network(network_obj)),
  network_obj
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
#> == Results =====================================================================
#> Duration: 0.2 s
#> 
#> OK:       6
#> Failed:   0
#> Warnings: 0
#> Skipped:  0
```
