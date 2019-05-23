#' Koschade's Jemaah Islamiyah (2002 Bali Bombings)
#' 
#' `jemmah_islamiyah` is an adaptation of the [UCINET](https://sites.google.com/site/ucinetsoftware/)
#' dataset "Jemaah Islamiyah Koschade". \cr
#' 
#' From the [UCINET documentation](https://sites.google.com/site/ucinetsoftware/datasets/covert-networks/jemaahislamiyahkoschade): \cr
#' 
#' Jemaah Islamiyah cell that was responsible for the Bali bombings 
#' in 2002 – i.e. should tally with the other Bali bombing dataset from JJATT. The 
#' recording of the interaction of the cell began following the  meeting in the Hotel 
#' Harem in Denpasar on October 6, when the group was considered to go ‘operationally 
#' covert’, and concluded when the majority of the group had left Bali before the 
#' implementation of the operation on October 11, 2002.
#' 
#' @format
#' * Object Class: `igraph`
#'   + directed: `FALSE`
#'   + bipartite: `FALSE`
#'   + multiplex: `FALSE`
#'   + vertices: `17`
#'   + edges: `63`
#'   + loops: `FALSE`
#' * Vertex Attributes:
#'   + `name`
#'     + `character` representing each persons's name.
#' * Edge Attributes:
#'   + `weight` 
#'     + `numeric` describing the strength of relations between individuals, ranging from
#'       `1` to `5`.
#'       + `1` refers to a weak relationship, such as a single text message or financial 
#'        transaction.
#'       + `5` refers to a strong relationship, such as individuals who resided together 
#'         or individuals who had numerous weak contacts over the period in question.
#' 
#' @source \url{https://sites.google.com/site/ucinetsoftware/datasets/covert-networks/jemaahislamiyahkoschade}
#' 
#' @references Koschade, Stuart (2006) A Social Network Analysis of Jemaah Islamiyah: The 
#' Applications to Counter-Terrorism and Intelligence. Studies in Conflict and Terrorism 
#' Vol. 29(6):pp. 559-575
#' @references International Crisis Group. 2003 ‘Jemaah Islamiyah in South East Asia: 
#' Damaged but Still Dangerous’
#' 
#' @examples 
#' library(snatools)
#' 
#' jemmah_islamiyah
#' 
#' as_network(jemmah_islamiyah)
#' 
"jemmah_islamiyah"
