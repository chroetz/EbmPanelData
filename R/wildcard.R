
expandWildcard <- function(word, vocabulary, wildcard = "*") {
  pattern <- paste0(
    "^\\Q",
    gsub(
      paste0("\\Q",wildcard,"\\E"),
      "\\\\E[a-zA-Z0-9]*\\\\Q",
      word),
    "\\E$")
  grep(pattern, vocabulary, value=TRUE)
}
