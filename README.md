
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
v |  2       | Round trip conversion: undirected graphs
#> 
/ |  0       | Round trip conversion: directed graphs
- |  1       | Round trip conversion: directed graphs
\ |  2       | Round trip conversion: directed graphs
v |  2       | Round trip conversion: directed graphs
#> 
/ |  0       | Round trip conversion: bipartite graphs
- |  0 1     | Round trip conversion: bipartite graphs
x |  0 1     | Round trip conversion: bipartite graphs
#> --------------------------------------------------------------------------------
#> test-round-trip-conversions.R:40: error: (unknown)
#> no applicable method for 'sna_standardize_graph' applied to an object of class "NULL"
#> 1: sna_standardize_graph() at C:\Users\Windows\Documents\snatools/tests/testthat/test-round-trip-conversions.R:40
#> --------------------------------------------------------------------------------
#> 
#> == Results =====================================================================
#> Duration: 0.2 s
#> 
#> OK:       4
#> Failed:   1
#> Warnings: 0
#> Skipped:  0
```
