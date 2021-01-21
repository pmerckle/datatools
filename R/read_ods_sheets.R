#' datatools::read_ods_sheets
#'
#'Import LibreOffice workbook with multiple sheets
#'
#'This function imports multiple sheets contained in a .ods spreadsheet into distinct tibbles.
#'
#' @param path path to the ods file.
#' @param skip An integer: number of rows to be skiped in the beginning of each sheet; default = 0
#' @return A series of tibbles corresponding to the different sheets contained in the Excel workbook.
#' @examples
#' \dontrun{
#' path <- ".../myworkbook.ods"
#' read_ods_sheets(path)
#' read_ods_sheets(path, skip = 2)
#' }
#' @import readODS
#' @export

read_ods_sheets <- function(path, skip = 0){

  sheetnames <- readODS::list_ods_sheets(path) # get sheetnames
  sheetlist <- sapply(sheetnames, function(x){readODS::read_ods(path, sheet = x, skip = 1)})
  for (sh in sheetnames) {
    sheetlist[[sh]] <- tibble:as_tibble(sheetlist[[sh]]) # create list containing each sheet
  }

  list2env(sheetlist, .GlobalEnv) # transfer list to Global Env.
}
