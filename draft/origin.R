# datatools::origin()
#
# This is the function named 'origin' and its variations,
# which get information about origin from first name
#



# origin_unique ----

#' Get origin of first name
#'
#' This function returns the geographic origin of a first name.
#'
#' @param first_name first name as a character string.
#' @return
#' The geographic origin of the first name. NA is returned when the input first name is unknown in the database.
#' @examples
#' origin_unique("Baptiste")
#' origin_unique("Steven")
#' @import dplyr
#' @import magrittr

origin_unique <- function(first_name) {
  temp <- fn_or %>% filter(fn == toupper(unaccent(first_name)))
  temp <- as.character(temp$origin[1])
  return(temp)
}


# origin ----

#' Get origins of first names
#'
#' This function returns the geographic origins of first names.
#'
#' @param firstname first names as a character string.
#' @return
#' The geographic origins of the first names. NA is returned when the input first names are unknown in the database.
#' @examples
#' origin(c("Baptiste", "Martha", "Lars", "Ahmed"))
#' @import dplyr
#' @import magrittr
#' @export

origin <- function(firstname) as.vector(sapply(firstname, origin_unique))

origin("Jessica")

gender("Sofiane", freq = TRUE, )
