#' @importFrom magrittr %>%
#' 
#' @export
#' 
magrittr::`%>%`


#' Test if objects are `identical`.
#' 
#' This infix operator provides a stricter method of comparing objects while preventing
#' surprise type conversions.
#' 
#' @param lhs left-hand side
#' @param rhs right-hand side
#' 
#' @return `logical` Whether `lhs` and `rhs` are `identical()`.
#' 
#' @seealso [`base::identical()`]
#' 
#' @author Brendan Knapp \email{brendan.g.knapp@@gmail.com}
#' 
#' @examples 
#' 
#' TRUE == 1L
#' TRUE == 1.0000000000000001
#' TRUE %==% 1L
#' TRUE %==% 1.0000000000000001
#' 
#' NA == NA_character_
#' NA %==% NA_character_
#' 
#' NULL == NA_integer_
#' NULL %==% NA_integer_
#' 
#' NA == "NA"
#' NA %==% "NA"
#' 
#' @export
`%==%` <- function(lhs, rhs) {
  identical(lhs, rhs)
}
