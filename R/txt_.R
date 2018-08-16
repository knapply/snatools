#' Text manipulation tools.
#' 
#' The `txt_` family includes internal functions that are exported for convenience.
#' They are simply wrappers around `{base}` R `character`-related methods 
#' that use a syntax inspired by [stringi::stringi]/[stringr::stringr].
#' 
#' @param text `character`. Object to target.
#' @param pattern `character`. Regular expression for matches.
#' @param replacement `character`. Object to replace matched `pattern`. 
#' @param ignore_case `logical`. Whether case matching should be strict.
#' @param start `integer`. Position of first character.
#' @param end `integer`. Position of last character.
#' @param ... Additional arguments to be passed to or from `{base}` methods listed in
#' \href{#see-also}{__See Also__}.
#' 
#' @name txt_
#' 
#' @rdname character-manipulation-tools
#' 
#' @seealso [base::grep()], [base::grepl()], [base::regexpr()], 
#' [base::regmatches()], [base::sub()], [base::gsub()], [base::substring()],
#' [base::strsplit()]
#' 
#' @export

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_detect <- function(text, pattern, ignore_case = FALSE, ...) {
  grepl(pattern = pattern, x = text, ignore.case = ignore_case, perl = TRUE, ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_extract <- function(text, pattern, ignore_case = FALSE, ...) {
  matches <- regexpr(pattern, text, ignore.case = ignore_case, perl = TRUE, ...)
  regmatches(text, matches)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_extract_all <- function(text, pattern, ignore_case = FALSE, ...) {
  matches <- gregexpr(pattern, text, ignore.case = ignore_case, perl = TRUE, ...)
  regmatches(text, matches)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_replace <- function(text, pattern, replacement, ignore_case = FALSE, ...) {
  sub(pattern, replacement, x = text, ignore.case = ignore_case, perl = TRUE, ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_replace_all <- function(text, pattern, replacement, ignore_case = FALSE, ...) {
  gsub(pattern, replacement, x = text, ignore.case = ignore_case, perl = TRUE, ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_remove <- function(text, pattern, ignore_case = FALSE, ...) {
  txt_replace(text, pattern, replacement = "", ignore_case = ignore_case, ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_remove_all <- function(text, pattern, ignore_case = FALSE, ...) {
  txt_replace_all(text, pattern, replacement = "", ignore_case = ignore_case, ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_split <- function(text, pattern, ...) {
  strsplit(text, pattern, perl = TRUE, ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_trim <- function(text, ...) {
  txt_remove_all(text, pattern = "^\\s+|\\s+$", ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_squish <- function(text, ...) {
  txt_replace_all(text, "(?<=\\w)\\s+(?=\\s)", "", ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_to_lower <- function(text) {
  tolower(text)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_to_upper <- function(text) {
  toupper(text)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_sub <- function(text, start = 1L, end = nchar(text)) {
  substring(text, start, end)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_subset <- function(text, pattern, ...) {
  text[txt_detect(text, pattern)]
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_which <- function(text, pattern, ...) {
  which(txt_detect(text, pattern), ...)
}

#' @rdname character-manipulation-tools
#' 
#' @export
#' 
txt_to_title <- function(text) {
  words <- strsplit(text, " ")[[1]]
  paste(txt_to_upper(txt_sub(words, 1, 1)), txt_sub(words, 2),
        sep = "", collapse = " ")
}