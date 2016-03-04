# parsers.R

#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_extract

remove_newLines <- function(str) {
  str_replace_all(str, '[\\n\\r\\t]', '')
}

remove_spaces <- function(str) {
  str_replace_all(str, ' ', '')
}

max_min <- function(str) {
  str_extract(str, "(?i)(?:min|max)(?:imize)?")
}

get_objective <- function(str) {
  str_extract(str, "^.+(?=SubjectTo.+$)")
}

check_quadratic <- function(str) {
  str_detect(str, "^.+(?=\\[.+\\])")
}

get_c <- function(str) {
  str_extract(str, "^.+(?=\\[)") %>%
    str_extract("^.+(?=[+-]$)")
}

get_Q <- function(str) {
  str_extract(str, "(?<=^[+-]\\[).+(?=\\]/2$)")
}
