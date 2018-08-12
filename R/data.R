#' Padgett's Florentine Families
#' 
#' 
#' 
#' __Background__, _as described in UCINET IV's manual_
#' * Breiger & Pattison (1986), in their discussion of local role analysis, use a 
#'   subset of data on the social relations among Renaissance Florentine families 
#'   (person aggregates) collected by John Padgett from historical documents. The two 
#'   relations are business ties (PADGB - specifically, recorded financial ties such as
#'   loans, credits and joint partnerships) and marriage alliances (PADGM).
#' * As Breiger & Pattison point out, the original data are symmetrically coded. This is 
#'   acceptable perhaps for marital ties, but is unfortunate for the financial ties (which
#'   are almost certainly directed). To remedy this, the financial ties can be recoded as 
#'   directed relations using some external measure of power - for instance, a measure of 
#'   wealth. PADGW provides information on (1) each family's net wealth in 1427 (in 
#'   thousands of lira); (2) the number of priorates (seats on the civic council) held
#'   between 1282- 1344; and (3) the total number of business or marriage ties in the 
#'   total dataset of 116 families (see Breiger & Pattison (1986), p 239).
#' 
#' 
#' @references 
#' * Breiger R. and Pattison P. (1986). Cumulated social roles: The duality of persons and
#'  their algebras. Social Networks, 8, 215-256.
#' * Kent D. (1978). The rise of the Medici: Faction in Florence, 1426-1434. Oxford:
#'  Oxford University Press.
#'
#' @format `igraph`
#'
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}
#'
#' @examples
#' library(snatools)
#' library(ggraph, quietly = TRUE)
#' 
#' data("florence")
#' 
#' florence %>% 
#'   ggraph(layout = "fr") +
#'   geom_edge_fan(aes(color = type)) +
#'   geom_node_point() +
#'   geom_node_text(aes(label = name), repel = TRUE) +
#'   theme_void()
#'
"florence"

#' Davis' Southern Women
#' 
#' @references 
#' * Breiger R. (1974). The duality of persons and groups. Social Forces, 53, 181-190.
#' * Davis, A et al. (1941). Deep South. Chicago: University of Chicago Press.
#' 
#' @format `igraph`
#'  
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}
#' 
#' @examples 
#' 
"southern_women"

#' Sampson's Monastery
#' 
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}
#' 
#' @format `list` of `igraph` objects
#' 
#' @references 
#' * Breiger R., Boorman S. and Arabie P. (1975). An algorithm for clustering relational
#'   data with applications to social network analysis and comparison with 
#'   multidimensional scaling. Journal of Mathematical Psychology, 12, 328-383.
#' * Sampson, S. (1969). Crisis in a cloister. Unpublished doctoral dissertation, Cornell 
#'   University.
#'   
#' @examples 
#' par(mfrow = c(2, 5))
#' 
#' snatools::monastery %>% 
#'   purrr::iwalk(~ plot(.x, main = .y, asp = 0, edge.arrow.size = 0.25,
#'                layout = igraph::layout_with_fr))
#'
"monastery"