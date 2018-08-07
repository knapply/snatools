
<!-- README.Rmd generates README.md. -->
<!-- # snatools <img src="man/figures/logo.png" align="right" height="243px" width="211px" /> -->
[![](https://img.shields.io/badge/devel%20version-0.1.1-green.svg)](https://github.com/knapply/snatools)

<br>

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
#> == Results =====================================================================
#> Duration: 0.2 s
#> 
#> OK:       6
#> Failed:   0
#> Warnings: 0
#> Skipped:  0
```
