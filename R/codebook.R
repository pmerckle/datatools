# codebook
#
# This is the function named 'codebook' and its variations,
# which build codebooks from data frames
#


## FUNCTIONS ----



# codebook ----

#' Codebook
#'
#' This function builds PDF codebooks from data frames.
#'
#' @param file Input data file in RDS format
#' @return A PDF file stored in the working directory.
#' @examples
#' data(mtcars)
#' saveRDS(mtcars, "mtcars.rds")
#' codebook("mtcars.rds")
#' @import magrittr
#' @import rmarkdown
#' @export

# file <- "mtcars.rds"
codebook <- function(file, weights = NA, main = "Codebook", sub = deparse(substitute(file)), print_table = TRUE) {
  file <- system.file("templete.Rmd", package = 'datatools')
  print(file)
  render(file,
         output_file = paste0(str_remove(file, "\\.rds$"), ".pdf"),
         params = list(data = data, weights = weights, main = main, sub = sub, print_table = print_table)
  )
}

