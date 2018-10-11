#' Sampson's Monastery
#' 
#' `crisis_in_cloister` is an adaptation of the [Pajek](http://mrvar.fdv.uni-lj.si/pajek/)
#' dataset "Sampson.paj" ("Sampson.net" specifically). \cr
#' 
#' From the Pajek documentation: \cr
#' 
#' The data stem from an ethnographic study of community structure in a New England 
#' monastery by Samuel F. Sampson. The study describes several social relations among a 
#' group of men (novices) who were preparing to join a monastic order.
#' 
#' The data sets presented here contain the affect relations among the novices, which 
#' were collected by asking them to indicate whom they liked most and whom they liked 
#' least. The novices were asked for a first, second, and third choice on both 
#' questions. The social relations were measured at five moments in time. The fourth 
#' measurement (at time T4) took place one week before four of the novitiates were 
#' expelled from the monastery. Some novices had attended the minor seminary of 
#' 'Cloisterville' before they came to the monastery.
#' 
#' Based on his observations and analyses, Sampson divided the novices into 
#' four groups: Young Turks, Loyal Opposition, Outcasts, and an interstitial group. The
#' Loyal Opposition consists of the novices who entered the monastery first. The Young 
#' Turks arrived later, in a period of change. They questioned practices in the 
#' monastery, which the members of the Loyal Opposition defended. Some novices did not 
#' take sides in this debate, so they are labeled 'interstitial'. The Outcasts are 
#' novices who were not accepted in the group.
#' 
#' @format
#' * Object Class: `igraph`
#'   + directed: `TRUE`
#'   + bipartite: `FALSE`
#'   + multiplex: `TRUE`
#'   + vertices: `25` ("novices")
#'   + edges: `401` ("affect relations")
#'   + loops: `FALSE`
#' * Vertex Attributes:
#'   + `name`
#'     + `character` representing each person's name.
#'   + `cloisterville`
#'     + `logical` indicating whether the person attended the minor seminary of 
#'       "Cloisterville" before coming to the monastery.
#' * Edge Attributes:
#'   + `affect` 
#'     + `numeric` describing relations as follow:
#'       + `3`: most-liked peer
#'       + `2`: second most-liked peer
#'       + `1`: third most-liked peer
#'       + `-1`: third least-liked peer
#'       + `-2`: second least-liked peer
#'       + `-3`: least-liked peer
#'   + `time`
#'     + `numeric` indicating which of the 5 different time points the edge describes.
#' 
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/esna/sampson.htm}
#' 
#' @references S.F. Sampson, A Novitiate in a Period of Change. An Experimental and Case 
#' Study of Social Relationships (PhD thesis Cornell University, 1968)
#' @references W. de Nooy, A. Mrvar, & V. Batagelj, Exploratory Social Network Analysis 
#' with Pajek (Cambridge: Cambridge University Press, 2004), Chapter 4.
#' 
#' @examples
#' library(snatools)
#' 
#' crisis_in_cloister
#' 
#' as_network(crisis_in_cloister)
#' 
"crisis_in_cloister"