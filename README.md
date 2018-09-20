
<!-- README.Rmd generates README.md. -->
snatools <a href="man/figures/logo.png"> <img src="man/figures/logo.png" align="right" height="45%" width="45%" href="man/figures/logo.png"/> </a>
==================================================================================================================================================

[![](https://img.shields.io/badge/devel%20version-0.0.0.9-red.svg)](https://github.com/knapply/snatools)

An R toolkit to bridge graph classes and streamline network analytic workflows.

<br>

Installation
------------

``` r
# Install {devtools} if you haven't already.
# install.packages("devtools")
requireNamespace(devtools)

# Install {snatools} from GitHub.
devtools::install_github("knapply/snatools")

# Load {snatools}.
library(snatools)
```

<br>

`network` to `igraph`
=====================

``` r
network_object <- snatools:::build_test_graph("nw")

network_object %>%  
  as_sna_net()
```

<pre class="r-output"><code>#&gt; A directed, multiplex, 1-mode `sna_net`.
#&gt; - Contains &gt;0 loops and 0 isolates.
#&gt; $edges # first 3 of 22
#&gt;    .ego    .alter  edge_chr edge_int edge_dbl edge_lgl
#&gt;    &lt;.name&gt; &lt;.name&gt; &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_2  node_7  c        4        329.7702  TRUE   
#&gt;  | node_4  node_3  q        1        501.9975 FALSE   
#&gt;  | node_5  node_6  p        5        677.0945  TRUE   
#&gt; $vertices # first 3 of 10
#&gt;    .name   node_chr node_int node_dbl node_lgl
#&gt;    &lt;.name&gt; &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_1  c        7        31.66125 TRUE    
#&gt;  | node_2  q        5        30.26934 TRUE    
#&gt;  | node_3  p        3        15.90460 TRUE
</code></pre>
``` r
network_object %>% 
  as_igraph()
```

<pre class="r-output"><code>#&gt; IGRAPH bed93ed DN-- 10 22 -- 
#&gt; + attr: graph_chr (g/c), graph_lgl (g/l), graph_int (g/n),
#&gt; | graph_dbl (g/n), node_chr (v/c), node_int (v/n), node_dbl (v/n),
#&gt; | node_lgl (v/l), name (v/c), edge_chr (e/c), edge_int (e/n),
#&gt; | edge_dbl (e/n), edge_lgl (e/l)
#&gt; + edges from bed93ed (vertex names):
#&gt;  [1] node_2-&gt;node_7  node_4-&gt;node_3  node_5-&gt;node_6  node_5-&gt;node_7 
#&gt;  [5] node_5-&gt;node_8  node_5-&gt;node_10 node_7-&gt;node_1  node_7-&gt;node_7 
#&gt;  [9] node_7-&gt;node_8  node_7-&gt;node_9  node_8-&gt;node_9  node_2-&gt;node_7 
#&gt; [13] node_4-&gt;node_3  node_5-&gt;node_6  node_5-&gt;node_7  node_5-&gt;node_8 
#&gt; [17] node_5-&gt;node_10 node_7-&gt;node_1  node_7-&gt;node_7  node_7-&gt;node_8 
#&gt; + ... omitted several edges
</code></pre>
bipartite `network` to `igraph`
-------------------------------

``` r
bipartite_network <- snatools:::build_test_graph("nw", directed = FALSE, bipartite = TRUE)
bipartite_network %>% 
  as_sna_net()
```

<pre class="r-output"><code>#&gt; An undirected, multiplex, bipartite `sna_net`.
#&gt; - Contains 0 loops and &gt;0 isolates.
#&gt; $edges # first 3 of 4
#&gt;    .ego    .alter  edge_chr edge_int edge_dbl edge_lgl
#&gt;    &lt;.name&gt; &lt;.name&gt; &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_1  node_3  c        4        666.0838  TRUE   
#&gt;  | node_1  node_6  q        3        514.2511 FALSE   
#&gt;  | node_1  node_3  p        1        693.5913  TRUE   
#&gt; $vertices # first 3 of 10
#&gt;    .name   .actor node_chr node_int node_dbl node_lgl
#&gt;    &lt;.name&gt; &lt;lgl&gt;  &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_1   TRUE  c        7        31.66125 TRUE    
#&gt;  | node_2   TRUE  q        5        30.26934 TRUE    
#&gt;  | node_3  FALSE  p        3        15.90460 TRUE
</code></pre>
``` r
bipartite_network %>% 
  as_igraph()
```

<pre class="r-output"><code>#&gt; IGRAPH bee3285 UN-B 10 4 -- 
#&gt; + attr: graph_chr (g/c), graph_lgl (g/l), graph_int (g/n),
#&gt; | graph_dbl (g/n), node_chr (v/c), node_int (v/n), node_dbl (v/n),
#&gt; | node_lgl (v/l), type (v/l), name (v/c), edge_chr (e/c), edge_int
#&gt; | (e/n), edge_dbl (e/n), edge_lgl (e/l)
#&gt; + edges from bee3285 (vertex names):
#&gt; [1] node_1--node_3 node_1--node_6 node_1--node_3 node_1--node_6
</code></pre>
`igraph` to `network`
=====================

``` r
igraph_object <- snatools:::build_test_graph("ig")

igraph_object %>% 
  as_sna_net()
```

<pre class="r-output"><code>#&gt; A directed, multiplex, 1-mode `sna_net`.
#&gt; - Contains &gt;0 loops and 0 isolates.
#&gt; $edges # first 3 of 22
#&gt;    .ego    .alter  edge_chr edge_int edge_dbl edge_lgl
#&gt;    &lt;.name&gt; &lt;.name&gt; &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_2  node_7  c        4        329.7702  TRUE   
#&gt;  | node_4  node_3  q        1        501.9975 FALSE   
#&gt;  | node_5  node_6  p        5        677.0945  TRUE   
#&gt; $vertices # first 3 of 10
#&gt;    .name   node_chr node_int node_dbl node_lgl
#&gt;    &lt;.name&gt; &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_1  c        7        31.66125 TRUE    
#&gt;  | node_2  q        5        30.26934 TRUE    
#&gt;  | node_3  p        3        15.90460 TRUE
</code></pre>
``` r
igraph_object %>% 
  as_network()
```

<pre class="r-output"><code>#&gt;  Network attributes:
#&gt;   vertices = 10 
#&gt;   directed = TRUE 
#&gt;   hyper = FALSE 
#&gt;   loops = TRUE 
#&gt;   multiple = TRUE 
#&gt;   bipartite = FALSE 
#&gt;   graph_chr = Much Graph. Many Attributes 
#&gt;   graph_lgl = TRUE 
#&gt;   graph_int = 1 
#&gt;   graph_dbl = 3.14 
#&gt;   total edges= 22 
#&gt;     missing edges= 0 
#&gt;     non-missing edges= 22 
#&gt; 
#&gt;  Vertex attribute names: 
#&gt;     node_chr node_dbl node_int node_lgl vertex.names 
#&gt; 
#&gt;  Edge attribute names: 
#&gt;     edge_chr edge_dbl edge_int edge_lgl
</code></pre>
bipartite `igraph` to `network`
-------------------------------

``` r
bipartite_igraph <- snatools:::build_test_graph("ig", directed = FALSE, bipartite = TRUE)

bipartite_igraph %>% 
  as_sna_net()
```

<pre class="r-output"><code>#&gt; An undirected, multiplex, bipartite `sna_net`.
#&gt; - Contains 0 loops and &gt;0 isolates.
#&gt; $edges # first 3 of 4
#&gt;    .ego    .alter  edge_chr edge_int edge_dbl edge_lgl
#&gt;    &lt;.name&gt; &lt;.name&gt; &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_1  node_3  c        4        666.0838  TRUE   
#&gt;  | node_1  node_6  q        3        514.2511 FALSE   
#&gt;  | node_1  node_3  p        1        693.5913  TRUE   
#&gt; $vertices # first 3 of 10
#&gt;    .name   .actor node_chr node_int node_dbl node_lgl
#&gt;    &lt;.name&gt; &lt;lgl&gt;  &lt;chr&gt;    &lt;int&gt;    &lt;dbl&gt;    &lt;lgl&gt;   
#&gt;  | node_1   TRUE  c        7        31.66125 TRUE    
#&gt;  | node_2   TRUE  q        5        30.26934 TRUE    
#&gt;  | node_3  FALSE  p        3        15.90460 TRUE
</code></pre>
``` r
bipartite_igraph %>% 
  as_network()
```

<pre class="r-output"><code>#&gt;  Network attributes:
#&gt;   vertices = 10 
#&gt;   directed = FALSE 
#&gt;   hyper = FALSE 
#&gt;   loops = FALSE 
#&gt;   multiple = TRUE 
#&gt;   bipartite = 2 
#&gt;   graph_chr = Much Graph. Many Attributes 
#&gt;   graph_lgl = TRUE 
#&gt;   graph_int = 1 
#&gt;   graph_dbl = 3.14 
#&gt;   total edges= 4 
#&gt;     missing edges= 0 
#&gt;     non-missing edges= 4 
#&gt; 
#&gt;  Vertex attribute names: 
#&gt;     .actor node_chr node_dbl node_int node_lgl vertex.names 
#&gt; 
#&gt;  Edge attribute names: 
#&gt;     edge_chr edge_dbl edge_int edge_lgl
</code></pre>
Development Tests
-----------------

``` r
devtools::test()
```

<pre class="r-output"><code>#&gt; Loading snatools
</code></pre>
<pre class="r-output"><code>#&gt; Loading required package: testthat
</code></pre>
<pre class="r-output"><code>#&gt; Testing snatools
</code></pre>
<pre class="r-output"><code>#&gt; v | OK <span style='color: #BB0000;'>F</span><span> </span><span style='color: #BB00BB;'>W</span><span> </span><span style='color: #0000BB;'>S</span><span> | Context
#&gt; 
/ |  0       | converting empty graphs to `sna_net`s
- |  1       | converting empty graphs to `sna_net`s
\ |  2       | converting empty graphs to `sna_net`s
| |  3       | converting empty graphs to `sna_net`s
/ |  4       | converting empty graphs to `sna_net`s
</span><span style='color: #00BB00;'>v</span><span> |  4       | converting empty graphs to `sna_net`s</span><span style='color: #00BBBB;'> [0.2 s]</span><span>
#&gt; 
/ |  0       | converting non-empty graphs to `sna_net`s
- |  1       | converting non-empty graphs to `sna_net`s
\ |  2       | converting non-empty graphs to `sna_net`s
| |  3       | converting non-empty graphs to `sna_net`s
/ |  4       | converting non-empty graphs to `sna_net`s
</span><span style='color: #00BB00;'>v</span><span> |  4       | converting non-empty graphs to `sna_net`s
#&gt; 
/ |  0       | `igraph` %&gt;% `network` vertices match control
- |  1       | `igraph` %&gt;% `network` vertices match control
\ |  2       | `igraph` %&gt;% `network` vertices match control
| |  3       | `igraph` %&gt;% `network` vertices match control
/ |  4       | `igraph` %&gt;% `network` vertices match control
</span><span style='color: #00BB00;'>v</span><span> |  4       | `igraph` %&gt;% `network` vertices match control
#&gt; 
/ |  0       | `network` %&gt;% `igraph` vertices match control
- |  1       | `network` %&gt;% `igraph` vertices match control
\ |  2       | `network` %&gt;% `igraph` vertices match control
| |  3       | `network` %&gt;% `igraph` vertices match control
/ |  4       | `network` %&gt;% `igraph` vertices match control
</span><span style='color: #00BB00;'>v</span><span> |  4       | `network` %&gt;% `igraph` vertices match control
#&gt; 
/ |  0       | edg_attr_names()
- |  1       | edg_attr_names()
\ |  2       | edg_attr_names()
| |  3       | edg_attr_names()
</span><span style='color: #00BB00;'>v</span><span> |  3       | edg_attr_names()
#&gt; 
/ |  0       | edg_get_attr()
- |  1       | edg_get_attr()
\ |  2       | edg_get_attr()
| |  3       | edg_get_attr()
</span><span style='color: #00BB00;'>v</span><span> |  3       | edg_get_attr()
#&gt; 
/ |  0       | edg_get_attr_df()
- |  1       | edg_get_attr_df()
\ |  2       | edg_get_attr_df()
| |  3       | edg_get_attr_df()
</span><span style='color: #00BB00;'>v</span><span> |  3       | edg_get_attr_df()
#&gt; 
/ |  0       | raw matrix edgelists using indices
- |  1       | raw matrix edgelists using indices
\ |  2       | raw matrix edgelists using indices
| |  3       | raw matrix edgelists using indices
</span><span style='color: #00BB00;'>v</span><span> |  3       | raw matrix edgelists using indices
#&gt; 
/ |  0       | raw matrix edgelists using names
- |  1       | raw matrix edgelists using names
\ |  2       | raw matrix edgelists using names
| |  3       | raw matrix edgelists using names
</span><span style='color: #00BB00;'>v</span><span> |  3       | raw matrix edgelists using names
#&gt; 
/ |  0       | raw matrix edgelists using vertex attributes
- |  1       | raw matrix edgelists using vertex attributes
\ |  2       | raw matrix edgelists using vertex attributes
| |  3       | raw matrix edgelists using vertex attributes
</span><span style='color: #00BB00;'>v</span><span> |  3       | raw matrix edgelists using vertex attributes
#&gt; 
/ |  0       | edgelist class objects using indices
- |  1       | edgelist class objects using indices
\ |  2       | edgelist class objects using indices
| |  3       | edgelist class objects using indices
</span><span style='color: #00BB00;'>v</span><span> |  3       | edgelist class objects using indices
#&gt; 
/ |  0       | edgelist class objects using names
- |  1       | edgelist class objects using names
\ |  2       | edgelist class objects using names
| |  3       | edgelist class objects using names
</span><span style='color: #00BB00;'>v</span><span> |  3       | edgelist class objects using names
#&gt; 
/ |  0       | edgelist class objects using vertex attributes
- |  1       | edgelist class objects using vertex attributes
\ |  2       | edgelist class objects using vertex attributes
| |  3       | edgelist class objects using vertex attributes
</span><span style='color: #00BB00;'>v</span><span> |  3       | edgelist class objects using vertex attributes
#&gt; 
/ |  0       | net_is_directed
- |  1       | net_is_directed
\ |  2       | net_is_directed
| |  3       | net_is_directed
/ |  4       | net_is_directed
- |  5       | net_is_directed
\ |  6       | net_is_directed
</span><span style='color: #00BB00;'>v</span><span> |  6       | net_is_directed
#&gt; 
/ |  0       | net_is_bipartite
- |  1       | net_is_bipartite
\ |  2       | net_is_bipartite
| |  3       | net_is_bipartite
/ |  4       | net_is_bipartite
- |  5       | net_is_bipartite
\ |  6       | net_is_bipartite
</span><span style='color: #00BB00;'>v</span><span> |  6       | net_is_bipartite
#&gt; 
/ |  0       | edge counts
- |  1       | edge counts
\ |  2       | edge counts
| |  3       | edge counts
/ |  4       | edge counts
- |  5       | edge counts
\ |  6       | edge counts
</span><span style='color: #00BB00;'>v</span><span> |  6       | edge counts
#&gt; 
/ |  0       | vertex counts
- |  1       | vertex counts
\ |  2       | vertex counts
| |  3       | vertex counts
/ |  4       | vertex counts
- |  5       | vertex counts
\ |  6       | vertex counts
</span><span style='color: #00BB00;'>v</span><span> |  6       | vertex counts
#&gt; 
#&gt; == </span><span style='font-weight: bold;'>Results</span><span> =====================================================================
#&gt; </span><span style='color: #00BBBB;'>Duration: 0.7 s</span><span>
#&gt; 
#&gt; OK:       </span><span style='color: #00BB00;'>67</span><span>
#&gt; Failed:   </span><span style='color: #00BB00;'>0</span><span>
#&gt; Warnings: </span><span style='color: #00BB00;'>0</span><span>
#&gt; Skipped:  </span><span style='color: #00BB00;'>0</span><span>
</span></code></pre>
