# utils
#
# Utilitary functions
#



# install_library ----

#' Installation and Loading of Packages
#'
#' Checks whether packages are installed before loading
#'
#' @param pkgs character vector of the names of packages to install (if they are not already installed) and load.
#' @details
#' The function first checks whether packages are installed, and downloads from the CRAN repository those which are not already present, before loading them by calling the library() function.
#' @seealso \code{\link[utils]{install.packages}}, \code{\link[base]{library}}
#' @examples
#' install_library("datatools")
#' install_library(c("questionr", "explor")
#' @export

install_library <- function(packages) {
  for (i in packages){
    if(!is.element(i, .packages(all.available = TRUE))) install.packages(i, dep = TRUE)
    library(i,character.only = TRUE)
  }
}

