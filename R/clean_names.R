#' clean.names
#'
#' A function for removing ALL punctuation, symbols, and spaces from string
#' from elements of vector "vec"
#' 
#' @param str String to be cleaned.

clean.names <- function(str) {
  # split components of interactions
  x <- strsplit(str, ":")[[1]]
  # and apply cleaning separately,
  # removing any punctuation (P), symbols (S), and separators (Z)
  x <- gsub("[\\p{P}\\p{S}\\p{Z}]", "", x, perl = T)
  # re-attach
  paste(x, collapse = ":")
}
clean.names <- Vectorize(clean.names, vectorize.args = ("str"), USE.NAMES = F)