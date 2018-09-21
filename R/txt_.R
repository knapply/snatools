txt_detect <- function(text, pattern, ignore_case = FALSE, ...) {
  grepl(pattern = pattern, x = text, ignore.case = ignore_case, perl = TRUE, ...)
}

txt_extract <- function(text, pattern, ignore_case = FALSE, ...) {
  matches <- regexpr(pattern, text, ignore.case = ignore_case, perl = TRUE, ...)
  regmatches(text, matches)
}

txt_extract_all <- function(text, pattern, ignore_case = FALSE, ...) {
  matches <- gregexpr(pattern, text, ignore.case = ignore_case, perl = TRUE, ...)
  regmatches(text, matches)
}


txt_replace <- function(text, pattern, replacement, ignore_case = FALSE, ...) {
  sub(pattern, replacement, x = text, ignore.case = ignore_case, perl = TRUE, ...)
}


txt_replace_all <- function(text, pattern, replacement, ignore_case = FALSE, ...) {
  gsub(pattern, replacement, x = text, ignore.case = ignore_case, perl = TRUE, ...)
}


txt_remove <- function(text, pattern, ignore_case = FALSE, ...) {
  txt_replace(text, pattern, replacement = "", ignore_case = ignore_case, ...)
}


txt_remove_all <- function(text, pattern, ignore_case = FALSE, ...) {
  txt_replace_all(text, pattern, replacement = "", ignore_case = ignore_case, ...)
}


txt_split <- function(text, pattern, ...) {
  strsplit(text, pattern, perl = TRUE, ...)
}


txt_trim <- function(text, ...) {
  txt_remove_all(text, pattern = "^\\s+|\\s+$", ...)
}


txt_squish <- function(text, ...) {
  txt_replace_all(text, "(?<=\\w)\\s+(?=\\s)", "", ...)
}


txt_to_lower <- function(text) {
  tolower(text)
}


txt_to_upper <- function(text) {
  toupper(text)
}


txt_sub <- function(text, start = 1L, end = nchar(text)) {
  substring(text, start, end)
}


txt_subset <- function(text, pattern, ...) {
  text[txt_detect(text, pattern)]
}


txt_which <- function(text, pattern, ...) {
  which(txt_detect(text, pattern), ...)
}


txt_to_title <- function(text) {
  words <- strsplit(text, " ")[[1]]
  paste(txt_to_upper(txt_sub(words, 1, 1)), txt_sub(words, 2),
        sep = "", collapse = " ")
}


txt_wrap <- function(text, width = floor(options("width")[["width"]] * 0.9), 
                     indent = 0, exdent = 0, initial = "", prefix = "") {
  out <- strwrap(text, width = width, indent = indent, exdent = exdent, initial = initial,
                 prefix = prefix, simplify = FALSE)
  vapply(out, paste0, character(1), collapse = ("\n"))
}


patch <- function(text, ..., initial = "", prefix = "") {
  txt_wrap(sprintf(text, ...), initial = initial, prefix = prefix)
}


cat_patch <- function(text, ..., initial = "", prefix = "") {
  cat(patch(text, initial = initial, prefix = prefix, ...))
}

