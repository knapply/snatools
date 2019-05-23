#' Padgett's Florentine Families
#' 
#' `florence` is an adaptation of the [Pajek](http://mrvar.fdv.uni-lj.si/pajek/)
#' dataset "Padgett.paj". \cr
#' 
#' From the Pajek documentation (originally from the UCINET IV documentation): \cr
#' 
#' Breiger & Pattison (1986), in their discussion of local role analysis, use a subset of 
#' data on the social relations among Renaissance Florentine families (person aggregates) 
#' collected by John Padgett from historical documents. The two relations are business 
#' ties (PADGB - specifically, recorded financial ties such as loans, credits and joint 
#' partnerships) and marriage alliances (PADGM).
#' 
#' As Breiger & Pattison point out, the original data are symmetrically coded. This is 
#' acceptable perhaps for marital ties, but is unfortunate for the financial ties (which
#' are almost certainly directed). To remedy this, the financial ties can be recoded as 
#' directed relations using some external measure of power - for instance, a measure of 
#' wealth. PADGW provides information on (1) each family's net wealth in 1427 (in 
#' thousands of lira); (2) the number of priorates (seats on the civic council) held 
#' between 1282- 1344; and (3) the total number of business or marriage ties in the total 
#' dataset of 116 families (see Breiger & Pattison (1986), p 239).
#' 
#' Substantively, the data include families who were locked in a struggle for political 
#' control of the city of Florence in around 1430. Two factions were dominant in this 
#' struggle: one revolved around the infamous Medicis (9), the other around the powerful 
#' Strozzis (15).
#' 
#' @format
#' * Object Class: `igraph`
#'   + directed: `FALSE`
#'   + bipartite: `FALSE`
#'   + multiplex: `TRUE`
#'   + vertices: `16`
#'   + edges: `35`
#'   + loops: `FALSE`
#' * Vertex Attributes:
#'   + `name`
#'     + `character` representing each family's name.
#'    + `wealth`
#'      + `numeric` indicating each family's net wealth in thousands of lira.
#'   + `priorate_seats` indicating the number of seats on the civic council each family held.
#' * Edge Attributes:
#'   + `relation` 
#'     + `character` describing relations as either `"business"` or `"marriage"`.
#' 
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/WaFa/default.htm}
#' 
#' @references Breiger R. and Pattison P. (1986). Cumulated social roles: The duality of 
#' persons and their algebras. Social Networks, 8, 215-256.
#' @references Kent D. (1978). The rise of the Medici: Faction in Florence, 1426-1434. 
#' Oxford: Oxford University Press.
#' 
#' @examples 
#' library(snatools)
#' 
#' florence
#' 
#' as_network(florence)
#' 
"florence"
