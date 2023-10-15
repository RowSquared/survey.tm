#' Pull data from ODK Google Sheet
#'
#' Pulls sheets from google sheet on which the questionnaire is developed.
#'
#' @param gs Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#' @param sheets Character vector of sheets to be read from `gs`
#'
#' @return List of data.tables which are the respective sheets of worksheet
#' @export
#'
get_odk_wsheet <- function(gs = "",
                           sheets = c("survey", "choices")) {
  # TODO: MAKE WARNING IF SOMEONE USES WHITESPACE INSTEAD OF UNDERSCORE FOR constraint message

  # TODO: ASSERT INPUT. CHECK IF gs IS gs4 CONFORM (gs4 has util function)
  # TODO: DONT RELY ON stats PACKAGE FOR SETNAMES. OVERALL BETTER WAY TO NAME LISTS? SIMPLY PLACE NAMES IN CHARACTER VECTOR THEN LAPPY?
  # GET METDATA FOR USE LATER ON
  sheet_meta <- googlesheets4::gs4_get(gs)

  # GET ALL SHEETS
  list <- lapply(sheets, \(sheet) {
    if (sheet %in% sheet_meta$sheets$name) {
      data.table::as.data.table(
        googlesheets4::read_sheet(gs, sheet = sheet)
      )
    }
  })
  # SET NAMES OF LIST ELEMENTS
  list <- stats::setNames(list, c(paste(sheet_meta$name, sheets, sep = "_")))

  return(list)
}





#' Transform list of ODK sheets into data.table of text items that shall be translated
#'
#' @param list List of sheets (as produced by \link[survey.tm]{get_odk_wsheet}that contain text items
#' @param cols Columns on sheet(s) to keep for translation item
#'
#' @import data.table
#'
#' @return data.table of text items along with identifier of chronological order and questionnaire
#' @export
#'
get_odk_titems <- function(list, cols = c("label", "hint", "constraint_message")) {
  # CHECK INPUT
  assertthat::assert_that(is.list(list))

  # ITERATE OVER ALL ELEMENTS IN WORKSHEET LIST
  list <- rbindlist(
    lapply(seq(1, length(list)), \(element) {

      # HARMONIZE NAMES (REMOVE WHITESPACE TO UNDERSCORE)
      setnames(list[[element]], names(list[[element]]), stringr::str_replace_all(names(list[[element]]), "\\s", "_"))

      # CHECK WHICH COLS ARE IN THERE. SUBSET ONLY THOSE WHICH ACTUALLY EXIST
      cols.keep <- cols[cols %in% names(list[[element]])]
      assertthat::assert_that(!identical(cols.keep, character(0)),
                              msg=paste("No columns that match",paste(cols,collapse=", "), "in list element",names(list)[element],"\nCheck param 'cols'."))

      #GIVE WARNING IF NO "label" COL
      if (!any(grepl("label",cols.keep))) message(paste0("Attention. No column 'label' identified in list element ", names(list)[element]," which is quite unusual."))
      dt <- list[[element]][, .SD, .SDcols = cols.keep]

      #HARMONIZE NAMES IN CASE label::XXX WAS USED
      setnames(dt, gsub('::.+$', '', names(dt)))

      #CHECK THAT WE HAVE ONLY UNIQUE COLUMNS
      assertthat::assert_that(length(unique(names(dt)))==length(names(dt)),
                              msg=paste("Column names not unique using",paste(cols,collapse=", "), "in list element",names(list)[element],"\nCheck param 'cols'."
                                        ))

      # ADD QUESTIONNAIRE
      dt[, `:=`(questionnaire = stringr::str_remove(names(list)[element], paste(paste0("_", c("survey", "choices")), collapse = "|")))]
      # ADD SEQUENTIAL ID. BUT FOR NOW BASED ON TYPE OF SHEET. SMALL WORKAROUND TO KEEP CHOICES SEPERATELY. NEEDS REVISION IF SUSO INCLUDED
      if (grepl("_survey", names(list)[element])) {
        dt[, seq.id := 1:.N]
      } else if (!grepl("_survey", names(list)[element])) dt[, seq.id := 1000000:(.N + 999999)]
    }),
    fill = TRUE
  )

  #Return new list
  return(list)
}




#' Parse translation items returned from 'get_odk_titems' into clean data table
#'
#' @param dt Data table returned by get_odk_titems
#
#' @import data.table
#' @return Data table
#' @export
#'
parse_odk_titems <- function(dt) {
  # TODO: INCLUDE IN get_odk_items? AT LEAST OPTIONAL WITH DEFAULT TRUE
  # MELT INTO LONG
  dt <- melt(dt,
    id.vars = c("questionnaire", "seq.id"), variable.name = "type",
    na.rm = T
  )

  # CLEANUP - REMOVE ONLY PROPER WHITESPACE TO NOT REMOVE TABS AND NEWLINES
  create.unique.var(dt)
  #Remove '\r\n' if at end of string as it would not be added to Google Sheets
  create.unique.var(dt, regex="\\\r\\\n$",col="value.unique")

  # COLLAPSE
  dt <- collapse_titems(dt)

  return(dt)
}
