
<!-- README.Rmd generates README.md. -->
snatools <a href="man/figures/logo.png"> <img src="man/figures/logo.png" align="right" height="45%" width="45%" href="man/figures/logo.png"/> </a>
==================================================================================================================================================

[![](https://img.shields.io/badge/devel%20version-0.0.0.9-red.svg)](https://github.com/knapply/snatools)

An R toolkit to bridge graph classes and streamline network analytic workflows.

Installation
------------

``` r
# Install {devtools} if you haven't already.
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install {snatools} from GitHub.
devtools::install_github("knapply/snatools")

# Load {snatools}.
library(snatools)
```

Usage
-----

### Conversion

Here's are example `igraph` and `network` objects. They are both fairly complicated and contain vertex and edge attributes of several storage types.

``` r
library(snatools)

# igraph =================================================================================
ig_initial <- snatools:::build_test_graph("ig") 
ig_initial
#> IGRAPH 397f1a9 DN-- 30 212 -- Erdos renyi (gnp) graph
#> + attr: name (g/c), type (g/c), loops (g/l), p (g/n), name (v/c),
#> | node_character (v/c), node_integer (v/n), node_double (v/n),
#> | edge_character (e/c), edge_integer (e/n), edge_double (e/n)
#> + edges from 397f1a9 (vertex names):
#>  [1] vrt1 ->vrt30 vrt8 ->vrt1  vrt11->vrt1  vrt15->vrt1  vrt20->vrt1 
#>  [6] vrt26->vrt1  vrt3 ->vrt2  vrt5 ->vrt2  vrt12->vrt2  vrt15->vrt2 
#> [11] vrt24->vrt2  vrt29->vrt2  vrt6 ->vrt3  vrt7 ->vrt3  vrt14->vrt3 
#> [16] vrt16->vrt3  vrt17->vrt3  vrt21->vrt3  vrt4 ->vrt30 vrt5 ->vrt4 
#> [21] vrt9 ->vrt4  vrt17->vrt4  vrt20->vrt4  vrt25->vrt4  vrt26->vrt4 
#> [26] vrt5 ->vrt30 vrt7 ->vrt5  vrt11->vrt5  vrt14->vrt5  vrt15->vrt5 
#> + ... omitted several edges

# network ================================================================================
nw_initial <- snatools:::build_test_graph("nw") 
nw_initial
#>  Network attributes:
#>   vertices = 30 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 218 
#>     missing edges= 0 
#>     non-missing edges= 218 
#> 
#>  Vertex attribute names: 
#>     node_character node_double node_integer vertex.names 
#> 
#>  Edge attribute names: 
#>     edge_character edge_double edge_integer
```

In order to confirm that an `igraph` or `network` object can make a round-trip conversion with its internal data remaining intact, we want to standardize its contents. We do this using `clean_graph()`.

``` r
# igraph =================================================================================
ig_clean <- clean_graph(ig_initial)

ig_clean
#> IGRAPH 397f1a9 DN-- 30 212 -- Erdos renyi (gnp) graph
#> + attr: loops (g/l), name (g/c), p (g/n), type (g/c), name (v/c),
#> | node_character (v/c), node_double (v/n), node_integer (v/n),
#> | edge_character (e/c), edge_integer (e/n), edge_double (e/n)
#> + edges from 397f1a9 (vertex names):
#>  [1] vrt1 ->vrt30 vrt8 ->vrt1  vrt11->vrt1  vrt15->vrt1  vrt20->vrt1 
#>  [6] vrt26->vrt1  vrt3 ->vrt2  vrt5 ->vrt2  vrt12->vrt2  vrt15->vrt2 
#> [11] vrt24->vrt2  vrt29->vrt2  vrt6 ->vrt3  vrt7 ->vrt3  vrt14->vrt3 
#> [16] vrt16->vrt3  vrt17->vrt3  vrt21->vrt3  vrt4 ->vrt30 vrt5 ->vrt4 
#> [21] vrt9 ->vrt4  vrt17->vrt4  vrt20->vrt4  vrt25->vrt4  vrt26->vrt4 
#> [26] vrt5 ->vrt30 vrt7 ->vrt5  vrt11->vrt5  vrt14->vrt5  vrt15->vrt5 
#> + ... omitted several edges

# network ================================================================================
nw_clean <- clean_graph(nw_initial)

nw_clean
#>  Network attributes:
#>   vertices = 30 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 218 
#>     missing edges= 0 
#>     non-missing edges= 218 
#> 
#>  Vertex attribute names: 
#>     node_character node_double node_integer vertex.names 
#> 
#>  Edge attribute names: 
#>     edge_character edge_double edge_integer
```

If you didn't notice any changes, don't worry. `clean_graph()` is simply ensuring that metadata not strictly required by `igraph` or `network` is present and that attribute names are sorted alphabetically. It's very possible there will be no changes.

Once an object is clean, we can easily convert it using `as_igraph()` and `as_network()`.

``` r
# igraph to network ======================================================================
as_network(ig_clean) 
#>  Network attributes:
#>   vertices = 30 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   name = Erdos renyi (gnp) graph 
#>   p = 0.25 
#>   type = gnp 
#>   total edges= 212 
#>     missing edges= 0 
#>     non-missing edges= 212 
#> 
#>  Vertex attribute names: 
#>     node_character node_double node_integer vertex.names 
#> 
#>  Edge attribute names: 
#>     edge_character edge_double edge_integer

# network to igraph ======================================================================
as_igraph(nw_clean)
#> IGRAPH 398be76 DN-- 30 218 -- 
#> + attr: loops (g/l), name (v/c), node_character (v/c), node_double
#> | (v/n), node_integer (v/n), edge_character (e/c), edge_double
#> | (e/n), edge_integer (e/n)
#> + edges from 398be76 (vertex names):
#>  [1] node_3 ->node_1 node_4 ->node_1 node_10->node_1 node_11->node_1
#>  [5] node_14->node_1 node_16->node_1 node_19->node_1 node_23->node_1
#>  [9] node_27->node_1 node_9 ->node_2 node_11->node_2 node_19->node_2
#> [13] node_21->node_2 node_24->node_2 node_1 ->node_3 node_4 ->node_3
#> [17] node_8 ->node_3 node_18->node_3 node_26->node_3 node_27->node_3
#> [21] node_28->node_3 node_8 ->node_4 node_10->node_4 node_19->node_4
#> + ... omitted several edges
```

### Testing for Successful Conversion

If you're concerned that `as_igraph()` or `as_network()` will mangle your graph's contents, you can test its original state against the result of a round-trip conversion using `identical()`.

Note that there's a *small* catch with `igraph` objects. `igraph` objects store an environment pointer that cannot be duplicated. You can inspect it like so:

``` r
unclass(ig_clean)[[10]]
#> <environment: 0x0000000022301e40>
```

In order to test that two `igraph` objects are identical, we need to omit that pointer from the object.

To simplify things, {`snatools`} includes the `%==%` operator to test that objects are `identical()`. It is equipped with an `igraph` method to handle pointer omission for you.

Finally, we can test round-trip conversions against their original graphs.

``` r
# igraph =================================================================================
ig_clean %==% as_igraph(as_network(ig_clean))
#> [1] TRUE

# network ================================================================================
nw_clean %==% as_network(as_igraph(nw_clean))
#> [1] TRUE
```

Development Test Results
------------------------

``` r
devtools::test()
#> Loading snatools
#> Loading required package: testthat
#> 
#> Attaching package: 'testthat'
#> The following object is masked from 'package:dplyr':
#> 
#>     matches
#> The following object is masked from 'package:purrr':
#> 
#>     is_null
#> The following object is masked from 'package:igraph':
#> 
#>     compare
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
/ |  0       | Round trip conversion: simple graph
- |  1       | Round trip conversion: simple graph
v |  1       | Round trip conversion: simple graph
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
#> Duration: 0.3 s
#> 
#> OK:       17
#> Failed:   0
#> Warnings: 0
#> Skipped:  0
```
