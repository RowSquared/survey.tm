#' Pull data from ODK Google Sheet
#'
#' Pulls sheets from google sheet on which the questionnaire is developed.
#'
#' @param wsid Identifier of a Google Work Sheet
#' @param sheets Character vector of sheets to be read on `wisid`
#'
#' @return List of data.tables which are the respective sheets of worksheet
#' @export
#'
get_odk_wsheet <- function(
  wsid="", #MAIN WORKSHEET ID
  sheets=c("survey","choices") #DEFAULT SHEETS TO READ
) {
  #TODO: MAKE WARNING IF SOMEONE USES WHITESPACE INSTEAD OF UNDERSCORE FOR constraint message

  #TODO: ASSERT INPUT. CHECK IF wsid IS gs4 CONFORM (gs4 has util function)
  #TODO: DONT RELY ON stats PACKAGE FOR SETNAMES. OVERALL BETTER WAY TO NAME LISTS? SIMPLY PLACE NAMES IN CHARACTER VECTOR THEN LAPPY?
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





#' Transform list of ODK sheets into data.table of text items
#'
#' @param list List of sheets (as produced by \link[r2.tms]{get_odk_wsheet}that contain text items
#' @param cols Columns to keep for translation item
#'
#' @import data.table
#'
#' @return data.table of text items along with identifier of chronological order and instrument
#' @export
#'
get_odk_titems <- function(list, cols=c("label","hint", "constraint_message")){
  #CHECK INPUT
  assertthat::assert_that(is.list(list))

  #ITERATE OVER ALL ELEMENTS IN WORKSHEET LIST
  rbindlist(
    lapply(seq(1,length(list)), \(element) {

      #HARMONIZE NAMES (REMOVE WHITESPACE TO UNDERSCORE)
      setnames(wsheets[[element]],names(wsheets[[element]]),stringr::str_replace_all(names(wsheets[[element]]),"\\s","_"))

      #CHECK WHICH COLS ARE IN THERE. SUBSET ONLY THOSE WHICH ACTUALLY EXIST
      cols.keep <- cols[cols %in% names(list[[element]])]
      dt <- list[[element]][,.SD,.SDcols=cols.keep]
      #ADD INSTRUMENT
      dt[, `:=` (instrt=stringr::str_remove(names(list)[element],paste(paste0("_",c("survey","choices")),collapse="|")))]

      #ADD SEQUENTIAL ID. BUT FOR NOW BASED ON TYPE OF SHEET. SMALL WORKAROUND TO KEEP CHOICES SEPERATELY. NEEDS REVISION IF SUSO INCLUDED
      if (grepl("_survey",names(list)[element])) dt[,seq.id:=1:.N]
      else if (!grepl("_survey",names(list)[element])) dt[,seq.id:=1000000:(.N+999999)]

    }), fill=TRUE

  )
}
