#' Padgett's Florentine Families
#' 
#' @details
#' __Background__, adapted from \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}: \cr
#' * Breiger & Pattison (1986), in their discussion of local role analysis, use a subset of 
#' data on the social relations among Renaissance Florentine families (person aggregates) 
#' collected by John Padgett from historical documents. The two relations are business
#' ties (specifically, recorded financial ties such as loans, credits and joint 
#' partnerships) and marriage alliances.
#'   
#' * As Breiger & Pattison point out, the original data are symmetrically coded. This is
#' acceptable perhaps for marital ties, but is unfortunate for the financial ties (which 
#' are almost certainly directed). To remedy this, the financial ties can be recoded as
#' directed relations using some external measure of power - for instance, a measure of 
#' wealth. PADGW provides information on (1) each family's net wealth in 1427 (in 
#' thousands of lira); (2) the number of priorates (seats on the civic council) held 
#' between 1282- 1344; and (3) the total number of business or marriage ties in the 
#' total dataset of 116 families (see Breiger & Pattison (1986), p 239).
#' 
#' * Subtantively, the data include families who were locked in a struggle for political 
#' control of the city of Florence in around 1430. Two factions were dominant in this 
#' struggle: one revolved around the infamous Medicis (9), the other around the powerful 
#' Strozzis (15).
#' 
#' @references 
#' * Breiger R. and Pattison P. (1986). Cumulated social roles: The duality of persons and
#'  their algebras. Social Networks, 8, 215-256.
#' * Kent D. (1978). The rise of the Medici: Faction in Florence, 1426-1434. Oxford:
#'  Oxford University Press.
#'
#' @format `network_dataset`
#'
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}
#'
#' @examples
#' data("padgett_florence", package = "snatools")
#' 
#' padgett_florence
#' 
#' as_igraph(padgett_florence)
#' 
#' as_network(padgett_florence)
#'
"padgett_florence"

#' Davis' Southern Women
#' 
#' @details
#' __Background__, adapted from \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}: \cr
#' * In the 1950s John Gagnon collected sociometric choice data from 67 prison inmates. All
#' were asked, "What fellows on the tier are you closest friends with?" Each was free to
#' choose as few or as many "friends" as he desired. The data were analyzed by MacRae and
#' characterized by him as "less clear cut" in their internal structure than similar data
#' from schools or residential populations.
#' 
#' @references 
#' * Breiger R. (1974). The duality of persons and groups. Social Forces, 53, 181-190.
#' * Davis, A et al. (1941). Deep South. Chicago: University of Chicago Press.
#' 
#' @format `network_dataset`
#'  
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}
#' 
#' @examples 
#' data("davis_southern_women", package = "snatools")
#' 
#' davis_southern_women
#' 
#' as_igraph(davis_southern_women)
#' 
#' as_network(davis_southern_women)
#' 
"davis_southern_women"

#' Sampson's Monastery
#' 
#' @details
#' __Background__, adapted from \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}: \cr
#' * Sampson recorded the social interactions among a group of monks while resident as an
#' experimenter on vision, and collected numerous sociometric rankings. During his stay, a 
#' political "crisis in the cloister" resulted in the expulsion of four monks (Nos. 2, 3, 
#' 17, and 18) and the voluntary departure of several others - most immediately, Nos. 1, 
#' 7, 14, 15, and 16. (In the end, only 5, 6, 9, and 11 remained).
#' 
#' * Most of the present data are retrospective, collected after the breakup occurred. They 
#' concern a period during which a new cohort entered the monastery near the end of the 
#' study but before the major conflict began. The exceptions are "liking" data gathered at
#' three times: SAMPLK1 to SAMPLK3 - that reflect changes in group sentiment over time 
#' (SAMPLK3 was collected in the same wave as the data described below). Information about 
#' the senior monks was not included.
#' 
#' * Four relations are coded, with separate matrices for positive and negative ties on the 
#' relation. Each member ranked only his top three choices on that tie. The relations are 
#' esteem (SAMPES) and disesteem (SAMPDES), liking (SAMPLK) and disliking (SAMPDLK), 
#' positive influence (SAMPIN) and negative influence (SAMPNIN), praise (SAMPPR) and blame 
#' (SAMPNPR). In all rankings 3 indicates the highest or first choice and 1 the last choice.
#' (Some subjects offered tied ranks for their top four choices).
#' 
#' 
#' @source \url{http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm}
#' 
#' @format `network_dataset`
#' 
#' @references 
#' * Breiger R., Boorman S. and Arabie P. (1975). An algorithm for clustering relational
#'   data with applications to social network analysis and comparison with 
#'   multidimensional scaling. Journal of Mathematical Psychology, 12, 328-383.
#' * Sampson, S. (1969). Crisis in a cloister. Unpublished doctoral dissertation, Cornell 
#'   University.
#'   
#' @examples 
#' data("sampson_monastery", package = "snatools")
#' 
#' sampson_monastery
#' 
#' as_igraph(sampson_monastery)
#' 
#' as_network(sampson_monastery)
#' 
"sampson_monastery"