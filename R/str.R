# str
#
# String functions
#



## FUNCTIONS ----



# unaccent ----

#' Remove accents
#'
#' This function removes accents from character strings.
#'
#' @param string input character vector.
#' @return An unaccented character vector.
#' @examples
#' unaccent("Jérémie")
#' unaccent("Héloïse")
#' @export

unaccent <- function(string) {
  text <- gsub("['`^~\"]", " ", string)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  return(text)
}

