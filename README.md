
<!-- README.Rmd generates README.md. -->

# snatools <a href="http://res.cloudinary.com/syknapptic/image/upload/v1537658876/logo_bnrvvg.png"> <img src="http://res.cloudinary.com/syknapptic/image/upload/v1537658876/logo_bnrvvg.png" align="right" height="24%" width="24%" href="http://res.cloudinary.com/syknapptic/image/upload/v1537658876/logo_bnrvvg.png"/> </a>

[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/knapply/snatools.svg?branch=master)](https://travis-ci.org/knapply/snatools)
[![codecov](https://codecov.io/gh/knapply/snatools/branch/master/graph/badge.svg)](https://codecov.io/gh/knapply/snatools)
[![Depends](https://img.shields.io/badge/Depends-GNU_R%3E=3.2-blue.svg)](https://www.r-project.org/)

<!-- [![GitHub Downloads](https://img.shields.io/github/downloads/knapply/snatools/total.svg)](https://github.com/knapply/snatools) -->

## Overview

`snatools` is still *very* experimental; the current API and
dependencies are subject to change.

`snatools` has four immediate goals meant to increase analyst efficiency
and reduce the cognitive load of network newcomers (and those
instructing them):

1.  Accurately bridge existing graph frameworks (i.e. `igraph` and
    `network`).
2.  Streamline network analytic workflows by making common tasks as
    intuitive (and stable) as possible.
3.  Implement metrics that are missing from R’s network ecosystem.
      - Reimplement those that *should* be made more accessible,
        efficient, or robust.
4.  Standardize fundamental tasks with methods that bypass the issues of
    `igraph` vs `network` namespace
conflicts.

<!-- Additionally, `snatools` seeks to prepare for the future by establishing the foundation (or a set of lessons-learned) for an enhanced network framework that: -->

<!-- 1. continues to be compatible with legacy frameworks. -->

<!-- 2. can be extended to take advantage of modern and future R's ecosystem . -->

<!--     + Optimization? `Rcpp`, C++'s Boost Graph Library, Python's graph-tool (heavily C++), CUDA -->

<!--     + Data Frame Efficiency? `data.table` -->

<!--     + Spatial Vector Data? `sf` -->

<!--     + Intuitive Code? `tidyverse`  -->

<!--         + `tidygraph` and `ggraph` may still be maturing, but they've rendered many legacy workflows for graph manipulation and plotting obsolete. -->

<!--     + Next Generation Tools? [Apache Arrow](https://arrow.apache.org/) is coming. -->

## Installation

``` r
# install.packages("devtools")
devtools::install_github("knapply/snatools")

library(snatools)
```

## Features

### Simple, Accurate Conversion Between Frameworks

`snatools` allows `igraph` and `network` to play together better than
ever.

`igraph` objects can be easily converted to `network` objects…

``` r
crisis_in_cloister %>% # an igraph object
  as_network()
```

    #>  Network attributes:
    #>   vertices = 25 
    #>   directed = TRUE 
    #>   hyper = FALSE 
    #>   loops = FALSE 
    #>   multiple = TRUE 
    #>   bipartite = FALSE 
    #>   total edges= 401 
    #>     missing edges= 0 
    #>     non-missing edges= 401 
    #> 
    #>  Vertex attribute names: 
    #>     .vrt_id cloisterville vertex.names 
    #> 
    #>  Edge attribute names: 
    #>     .edg_id affect time

… and back again.

``` r
crisis_in_cloister %>% 
  as_network() %>% 
  as_igraph()
```

    #> IGRAPH 975695a DN-- 25 401 -- 
    #> + attr: .vrt_id (v/n), name (v/c), cloisterville (v/l), .edg_id
    #> | (e/n), affect (e/n), time (e/n)
    #> + edges from 975695a (vertex names):
    #>  [1] John Bosco->Gregory     John Bosco->Gregory    
    #>  [3] John Bosco->Basil       John Bosco->Basil      
    #>  [5] John Bosco->Peter       John Bosco->Bonaventure
    #>  [7] John Bosco->Bonaventure John Bosco->Berthold   
    #>  [9] John Bosco->Mark        John Bosco->Mark       
    #> [11] John Bosco->Victor      John Bosco->Ramuald    
    #> [13] John Bosco->Ramuald     John Bosco->Ramuald    
    #> + ... omitted several edges

The same goes for bipartite graphs.

``` r
southern_women %>% # igraph to network
  as_network()
```

    #>  Network attributes:
    #>   vertices = 32 
    #>   directed = FALSE 
    #>   hyper = FALSE 
    #>   loops = FALSE 
    #>   multiple = FALSE 
    #>   bipartite = 18 
    #>   total edges= 89 
    #>     missing edges= 0 
    #>     non-missing edges= 89 
    #> 
    #>  Vertex attribute names: 
    #>     .actor .vrt_id vertex.names 
    #> 
    #>  Edge attribute names: 
    #>     .edg_id event_date event_day event_month

Additionally, everything in `snatools` was designed from the ground-up
to work with both `igraph` and `network` objects.

## Modern Subsetting

``` r
crisis_in_cloister %>% 
  edg_subset(time == 3 & affect > 0) %>% 
  vrt_subset(cloisterville)
```

    #> IGRAPH 9762267 DN-- 6 6 -- 
    #> + attr: .vrt_id (v/n), name (v/c), cloisterville (v/l), .edg_id
    #> | (e/n), affect (e/n), time (e/n)
    #> + edges from 9762267 (vertex names):
    #> [1] Peter  ->Berthold    Victor ->Peter       Victor ->Berthold   
    #> [4] Victor ->Ramuald     Ambrose->Bonaventure Ramuald->Bonaventure

## Standardized Representations

The essential graph `rep_`resentations are all available.

  - Adjacency Lists

<!-- end list -->

``` r
florence %>% 
  edg_subset(relation == "marriage") %>% 
  rep_as_adj_list()
```

    #> $Acciaiuoli
    #> [1] "Medici"
    #> 
    #> $Albizzi
    #> [1] "Ginori"   "Guadagni" "Medici"  
    #> 
    #> $Barbadori
    #> [1] "Castellani" "Medici"    
    #> 
    #> $Bischeri
    #> [1] "Guadagni" "Peruzzi"  "Strozzi" 
    #> 
    #> $Castellani
    #> [1] "Peruzzi"   "Strozzi"   "Barbadori"
    #> 
    #> $Ginori
    #> [1] "Albizzi"
    #> 
    #> $Guadagni
    #> [1] "Lamberteschi" "Tornabuoni"   "Albizzi"      "Bischeri"    
    #> 
    #> $Lamberteschi
    #> [1] "Guadagni"
    #> 
    #> $Medici
    #> [1] "Ridolfi"    "Salviati"   "Tornabuoni" "Acciaiuoli" "Albizzi"   
    #> [6] "Barbadori" 
    #> 
    #> $Pazzi
    #> [1] "Salviati"
    #> 
    #> $Peruzzi
    #> [1] "Strozzi"    "Bischeri"   "Castellani"
    #> 
    #> $Pucci
    #> [1] NA
    #> 
    #> $Ridolfi
    #> [1] "Strozzi"    "Tornabuoni" "Medici"    
    #> 
    #> $Salviati
    #> [1] "Medici" "Pazzi" 
    #> 
    #> $Strozzi
    #> [1] "Bischeri"   "Castellani" "Peruzzi"    "Ridolfi"   
    #> 
    #> $Tornabuoni
    #> [1] "Guadagni" "Medici"   "Ridolfi"

  - Edge Lists

<!-- end list -->

``` r
jemmah_islamiyah %>% 
  rep_as_edgelist() %>% 
  head()
```

    #>      .ego     .alter    
    #> [1,] "MUKLAS" "AMROZI"  
    #> [2,] "MUKLAS" "IMRON"   
    #> [3,] "MUKLAS" "SAMUDRA" 
    #> [4,] "MUKLAS" "DULMATIN"
    #> [5,] "MUKLAS" "IDRIS"   
    #> [6,] "MUKLAS" "AZAHARI"

  - Adjacency Matrices

<!-- end list -->

``` r
crisis_in_cloister %>%
  edg_subset(time == 2) %>% 
  rep_as_adj_matrix(edg_attr = "affect") %>% 
  `rownames<-`(regmatches(rownames(.), regexpr("^\\w{,4}", rownames(.)))) %>% 
  `colnames<-`(rep("", ncol(.)))
```

    #>                                                                        
    #> Leo   0 0 0 0 0 0  0  0 0  0  0  0  0 0  0  0  0  0  0  0  0 0  0  0  0
    #> Arsen 0 0 0 0 0 0  0  0 0  0  0  0  0 0  0  0  0  0  0  0  0 0  0  0  0
    #> Bruno 0 0 0 0 0 0  0  0 0  0  0  0  0 0  0  0  0  0  0  0  0 0  0  0  0
    #> Thoma 0 0 0 0 0 0  0  0 0  0  0  0  0 0  0  0  0  0  0  0  0 0  0  0  0
    #> Barth 0 0 0 0 0 0  0  0 0  0  0  0  0 0  0  0  0  0  0  0  0 0  0  0  0
    #> John  0 0 0 0 0 0  0  2 0  0  3 -2 -1 0  0  0 -3  0  0  0  1 0  0  0  0
    #> Grego 0 0 0 0 0 3  0  0 0  0  0  0  2 0  0  0 -1  0  0 -3  1 0  0 -2  0
    #> Basil 0 0 0 0 0 2  3  0 0 -1  0  0  0 0 -3 -2  0  0  0  0  0 0  0  1  0
    #> Marti 0 0 0 0 0 0  0  0 0  0  0  0  0 0  0  0  0  0  0  0  0 0  0  0  0
    #> Peter 0 0 0 0 0 0  0 -2 0  0  3  1 -3 0  0  0  2  0  0  0  0 0  0  0 -1
    #> Bonav 0 0 0 0 0 0  0  0 0  3  0  0  0 0  0  0  0  2  0  1  0 0  0  0  0
    #> Berth 0 0 0 0 0 1  0  0 0  3  0  0 -3 0 -1  2  0  0 -2  0  0 0  0  0  0
    #> Mark  0 0 0 0 0 0  2  0 0 -3 -1 -2  0 0  1  0  0  0  0  0  0 0  3  0  0
    #> Broca 0 0 0 0 0 0  0  0 0  0  0  0  0 0  0  0  0  0  0  0  0 0  0  0  0
    #> Victo 0 0 0 0 0 3  2 -3 0  0  0  0  0 0  0  1  0  0  0  0 -2 0  0 -1  0
    #> Ambro 0 0 0 0 0 0  0 -3 0  0  2  0  0 0  3  0  0  0  0  0  0 0  1 -2 -1
    #> Ramua 0 0 0 0 0 0  0  0 0  3  0  0  0 0  1  0  0  0  0  0  2 0  0  0  0
    #> Louis 0 0 0 0 0 0  0 -1 0  0  3  0  0 0  1  0  0  0  0 -3  2 0  0 -2  0
    #> Winfr 0 0 0 0 0 3  2 -1 0 -3  0  0  0 0  0  0  0 -2  0  0  1 0  0  0  0
    #> Amand 0 0 0 0 0 0 -3  0 0  0  2 -2  1 0  0  0  0  0 -1  0  0 0  0  0  3
    #> Hugh  0 0 0 0 0 3  0  0 0  0  0  0  0 0 -2  0  0  1  2 -3  0 2  0 -1  0
    #> Bonif 0 0 0 0 0 3  2 -2 0  0  0  0  0 0  0  0  0  0  0 -3  1 0  0 -1 -1
    #> Alber 0 0 0 0 0 1  2  0 0  0  0  0  3 0  0  0  0  0  0 -1  0 0  0 -3 -2
    #> Elias 0 0 0 0 0 0  0  3 0 -3 -2  0  0 0  0  0  0 -1  0  2  0 0  0  0  1
    #> Simpl 0 0 0 0 0 2  3  0 0 -3  0 -2  1 0  0  0  0  0  0  0  0 0 -1  0  0

Along with others that are less common.

  - Mixing Matrices

<!-- end list -->

``` r
data("samplk", package = "ergm")

samplk1 %>%
  rep_as_mixing_matrix(vrt_attr = "group")
```

    #>           Ties Received
    #> Ties Sent  Loyal Outcasts Turks Waverers
    #>   Loyal        8        0     3        4
    #>   Outcasts     0        3     5        1
    #>   Turks        2        1    18        1
    #>   Waverers     3        1     4        1

## Data Frame Methods

Modern R is all about data frames, so using them in network analysis
should be easy.

`tibble`s are the focus as the `statnet` suite already imports
`tidyverse` packages.

``` r
crisis_in_cloister %>% 
  vrt_as_df()
```

    #> # A tibble: 25 x 3
    #>    .vrt_id .vrt_name   cloisterville
    #>      <int> <chr>       <lgl>        
    #>  1       1 Leo         TRUE         
    #>  2       2 Arsenius    TRUE         
    #>  3       3 Bruno       TRUE         
    #>  4       4 Thomas      TRUE         
    #>  5       5 Bartholomew TRUE         
    #>  6       6 John Bosco  FALSE        
    #>  7       7 Gregory     FALSE        
    #>  8       8 Basil       FALSE        
    #>  9       9 Martin      TRUE         
    #> 10      10 Peter       TRUE         
    #> # ... with 15 more rows

``` r
crisis_in_cloister %>% 
  edg_as_df()
```

    #> # A tibble: 401 x 5
    #>    .edg_id  .ego .alter affect  time
    #>      <int> <int>  <int>  <int> <int>
    #>  1       1     6      7      2     3
    #>  2       2     6      7     -2     4
    #>  3       3     6      8      2     2
    #>  4       4     6      8      3     4
    #>  5       5     6     10     -2     3
    #>  6       6     6     11      1     3
    #>  7       7     6     11      3     2
    #>  8       8     6     12     -2     2
    #>  9       9     6     13     -1     2
    #> 10      10     6     13     -3     4
    #> # ... with 391 more rows

## More Metrics

``` r
library(ggplot2)

samplk1 %>% 
  ei_index(vrt_attr = "group", scope = "vertex") %>% 
  autoplot()
```

<img src="man/figures/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />
