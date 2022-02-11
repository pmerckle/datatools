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
#' @param path Path to the data frame directory
#' @param weights Variable containing values used to weight the dataset
#' @param main Main codebook title
#' @param sub Codebook subtitle
#' @param description Data frame description text
#' @param print_table Logical. Indicates whether frequency tables of categorical variables must be displayed
#' @return A PDF file stored in the same directory as the data frame.
#' @examples
#' data(mtcars)
#' codebook(mtcars)
#' @import magrittr
#' @import rmarkdown
#' @export

# file <- "mtcars.rds"
codebook <- function(df, path = getwd(), weights = NA, main = deparse(substitute(file)), sub = "Codebook", description = "", print_table = TRUE) {
  name <- deparse(substitute(df))
  output <- paste0(path, "/", name, ".pdf")
  render(system.file("rmd", "codebook_source.Rmd", package = 'datatools'),
    output_file = output,
    params = list(df = df, name = name, weights = weights, main = main, sub = sub, description = description, print_table = print_table),
    quiet = TRUE
  )
}

