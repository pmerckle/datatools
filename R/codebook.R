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
#' @param df A data frame
#' @return A PDF file stored in the working directory.
#' @examples
#' data(mtcars)
#' codebook(mtcars)
#' @import magrittr
#' @import rmarkdown
#' @export

# file <- "mtcars.rds"
codebook <- function(df, path = getwd(), weights = NA, main = deparse(substitute(file)), sub = "Codebook", print_table = TRUE) {
  name <- deparse(substitute(df))
  output <- paste0(path, "/", name, ".pdf")
  print(output)
  render(system.file("rmd", "codebook_source.Rmd", package = 'datatools'),
         output_file = output,
         params = list(df = df, name = name, weights = weights, main = main, sub = sub, print_table = print_table)
  )
}

