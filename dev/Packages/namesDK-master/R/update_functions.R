# This function updates the package data

update_from_csv <- function(){

  girls <- readr::read_csv("inst/extdata/girls.csv", col_names = F)
  names(girls) <- "names"

  boys <- readr::read_csv("inst/extdata/boys.csv", col_names = F)
  names(boys) <- "names"

  unisex <- readr::read_csv("inst/extdata/unisex.csv", col_names = F)
  names(unisex) <- "names"

  demographics <- readr::read_csv("inst/extdata/dataset.csv", col_names = T)

  devtools::use_data(girls, boys, unisex, demographics, overwrite = T, internal = T)

}

#############################################################################

#' Gender classifier for danish names
#'
#' The function uses the official danish lists of male, female and unisex first names
#' to classify names as either male, female or unisex. If a name can not be determined
#' as either one of those three it will return as NA.
#'
#' The functions is vectorized and can accept several names at ones. It accepts both first
#' names and full names. In case of full names it splits on space and choses the first name.
#'
#' @param names A vector of names - either one or several
#'
#' @return list of gender classifications
#'
#' @source https://ast.dk/born-familie/navne/navnelister/godkendte-fornavne
#'
#' @examples
#' library(namesDK)
#'
#' gender("Lars Løkke Rasmussen")
#'
#' gender(c("Helle Thorning Smidt", "Lars Løkke Rasmussen", "Traktor Troels"))
#'
#' @export

gender <- function(names){

  gender <- lapply(names, function(name){
    firstname <- unlist(stringr::str_split(name, " "))
    firstname <- stringr::str_to_lower(firstname[1])

    is.boy <- firstname %in% stringr::str_to_lower(boys$names)
    is.girl <- firstname %in% stringr::str_to_lower(girls$names)
    is.unisex <- firstname %in% stringr::str_to_lower(unisex$names)

    gender <- c("male", "female", "unisex")[c(is.boy, is.girl, is.unisex)]

    if(length(gender) == 0) gender <- NA
    if(length(gender) == 3) gender <- "unisex"

    return(gender)
  })

  return(gender)
}



