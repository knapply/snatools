
<!-- README.Rmd generates README.md. -->
snatools <a href="man/figures/logo.png"> <img src="man/figures/logo.png" align="right" height="45%" width="45%" href="man/figures/logo.png"/> </a>
==================================================================================================================================================

[![](https://img.shields.io/badge/devel%20version-0.0.0.9-red.svg)](https://github.com/knapply/snatools)

An R toolkit to bridge graph classes and streamline network analytic workflows.

Installation
------------

``` r
# Install {devtools} if you haven't already.
# install.packages("devtools")

require(devtools)

# Install {snatools} from GitHub.
install_github("knapply/snatools")

# Load {snatools}.
library(snatools)
```

Development Test Results
------------------------

| context                                    |  \# of tests| All Successful |
|:-------------------------------------------|------------:|:---------------|
| Directed `attr_el`s from `attr_adj_mats`   |            2| TRUE           |
| Directed Adjacency Matrices                |            5| TRUE           |
| Directed Attribute Adjacency Matrices      |            2| TRUE           |
| Directed Attribute Edge Lists              |            2| TRUE           |
| Directed Edge Lists                        |            8| TRUE           |
| Directed Mixing Matrices                   |            2| TRUE           |
| Undirected `attr_el`s from `attr_adj_mats` |            2| TRUE           |
| Undirected Adjacency Matrices              |            5| TRUE           |
| Undirected Attribute Adjacency Matrices    |            2| TRUE           |
| Undirected Attribute Edge Lists            |            2| TRUE           |
| Undirected Edge Lists                      |            8| TRUE           |
| Undirected Mixing Matrices                 |            2| TRUE           |
| Round trip conversion: bipartite graphs    |            3| TRUE           |
| Round trip conversion: directed graphs     |            2| TRUE           |
| Round trip conversion: simple graph        |            1| TRUE           |
| Round trip conversion: undirected graphs   |            2| TRUE           |
| E-I Index: directed                        |            2| TRUE           |
| E-I Index: undirected                      |            2| TRUE           |
