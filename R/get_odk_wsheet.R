#' Pull data from ODK Google Sheet
#'
#' Pulls sheets from google sheet on which the questionnaire is developed.
#'
#' @param wsid Identifier of a Google Work Sheet
#' @param sheets Character vector of sheets to be read on `wisid`
#'
#' @importFrom googlesheets4 gs4_get read_sheet
#' @importFrom data.table as.data.table
#' @return List of data.tables which are the respective sheets of worksheet
#' @export
#'
get_odk_wsheet <- function(
  wsid="", #MAIN WORKSHEET ID
  sheets=c("survey","choices") #DEFAULT SHEETS TO READ
) {
  #TODO: ASSERT INPUT. CHECK IF wsid IS gs4 CONFORM (gs4 has util function)
  #TODO: DONT RELY ON stats PACKAGE FOR SETNAMES. OVERALL BETTER WAY TO NAME LISTS?
  #GET METDATA FOR USE LATER ON
  sheet_meta <- googlesheets4::gs4_get(wsid)

  #GET ALL SHEETS
  list <- lapply(sheets,\(sheet) {
    if (sheet %in% sheet_meta$sheets$name) data.table::as.data.table(
      googlesheets4::read_sheet(wsid,  sheet=sheet)
    )
  })
  #SET NAMES OF LIST ELEMENTS
  list <- stats::setNames(list, c(paste(sheet_meta$name,sheets,sep="_")))

  return(list)
}
