#' datatools::read_xl_sheets
#'
#'Import Excel workbook with multiple sheets
#'
#'This function imports multiple sheets contained in a .xls/.xlsx spreadsheet into distinct tibbles.
#'
#' @param path path to the xls/xlsx file.
#' @param skip An integer: number of rows to be skiped in the beginning of each sheet; default = 0.
#' @return A series of tibbles corresponding to the different sheets contained in the Excel workbook.
#' @examples
#' \dontrun{
#' path <- ".../myworkbook.xls"
#' read_xl_sheets(path)
#' read_xl_sheets(path, skip = 2)
#' }
#' @import readxl
#' @export

read_xl_sheets <- function(path, skip = 0){

  sheetnames <- readxl::excel_sheets(path) # get sheetnames
  sheetlist <- sapply(sheetnames, function (x){readxl::read_excel(path, sheet = x, skip = skip)})
  for (sh in sheetnames) {
    sheetlist[[sh]] <- as_tibble(sheetlist[[sh]]) # create list containing each sheet
  }

  list2env(sheetlist, .GlobalEnv) # transfer list to Global Env.
}
