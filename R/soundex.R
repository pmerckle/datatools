# datatools::soundex()
#
# This is the function named 'soundex' and its variations,
# which code soundexes from names
#


## FUNCTIONS ----


# soundex ----

#' Phonetically encodes names using a soundex algorithm
#'
#' This function encodes names using a soundex algorithm.
#'
#' @param string input character vector.
#' @param method input character vector.
#' @return A character vector.
#' @examples
#' soundex("Moskovitz", method = "Daitch-Mokotoff")
#' soundex("Lubartow", method = "Daitch-Mokotoff")
#' @import stringr

soundex <- function(string, method = "Daitch-Mokotoff") {
  # Copy into result
  result <- name
  # Clean name encoding
  result[is.null(result)] <- NA
  result <- as.character(result)
  result <- enc2utf8(result)
  result <- toupper(result)
  # Remove accents
  result <- iconv(result, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Remove non-alphabet characters
  result <- gsub("[^A-Z]*", "", result)

  # Code the beginning of the name
  start <- as.character(dm_rules$start)
  names(start) <- paste0("^", dm_rules$pattern)
  # Replacements
  result <- str_replace_all(string = result, start)

  # Code the end of the name
  final <- as.character(dm_rules$final)
  names(final) <- paste0(dm_rules$pattern, "$")
  # Replacements
  result <- str_replace_all(string = result, final)

  # Code the sounds before a vowel
  vowel <- as.character(paste0(dm_rules$vowel, "\\1"))
  names(vowel) <- paste0(dm_rules$pattern, "([AEIOUY1.])")
  # Replacements
  result <- str_replace_all(string = result, vowel)

  # Code all others
  other <- as.character(dm_rules$other)
  names(other) <- dm_rules$pattern
  # Replacements
  result <- str_replace_all(string = result, other)

  # Code multiples
  multi <- function(string) {
    simple <- function(x) {
      x %>%
        str_split(pattern = "#") %>%
        unlist %>%
        str_split(pattern = "/") %>%
        expand.grid() %>%
        apply(1, paste, collapse = "") %>%
        sort %>%
        paste(collapse = "-")
    }
    lapply(string, simple) %>% unlist
  }
  result <- multi(result)

  # Code as one sound the adjacent letters that have the same code number (except 6 for NM and MN)
  result <- str_replace_all(result, pattern = "([012345789])\\1{1,}", replacement = "\\1")

  # Remove points
  result <- str_replace_all(result, pattern = "[.]*", replacement = "")

  # Cut/extend to 6 digits
  result <- result %>%
    str_split(pattern = "-") %>%
    lapply(function(x) str_sub(x, start = 1, end = 6)) %>%
    lapply(function(x) str_pad(x, width = 6, side = "right", pad = "0")) %>%
    lapply(function(x) paste(x, collapse = "-")) %>%
    unlist

  # Order and keep only distinct
  result <- result %>%
    str_split(pattern = "-") %>%
    lapply(unique) %>%
    lapply(sort) %>%
    lapply(paste, collapse = "-") %>%
    unlist

  # Output
  return(result)
}



