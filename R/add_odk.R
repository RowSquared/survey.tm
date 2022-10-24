
#' Add translation to ODK Questionnaire File
#'
#' @param trans.list
#' @param gs Identifier of a Google Sheet (see [googlesheets4::sheets_id()]) that contains ODK questionnaire file
#' @param sheets
#' @param translation.to.add
#' @param types
#'
#' @return
#' @export
#'
add_translation_odk <- function(trans.list = list(),
                                gs = NULL, # THE QUESTIONNAIRE SHEET OF ODK QX
                                sheets = c("survey", "choices"), # ON WHICH SHEET IN QX
                                translation.to.add = NULL, # WHICH TRANSLATION TO BE USED
                                types = NULL # WHICH COLUMNS TO BE ADDED. BY DEFAULT ALL THOSE WHICH FOUND IN TRANSATLION FILE
) {
  # TODO: COL. START IDENTIFY AUTOMATICALLY: IF COL EXISTS USE THAT OR IF NOT LAST ONE +1 IN METADATA
  # TODO: ASSERTIONS!
  # TODO: SPLIT UP? BIG FUNCTION AND HARD TO DIGEST
  #TODO: ADD FILTER WHICH TEXT ITEM STATUSES TO BE ADDED
  #TODO: PRINT WHICH TYPE IS ADDED AS COLUMN

  assertthat::assert_that(translation.to.add %in% names(trans.list),
    msg = paste(translation.to.add, "is not an element in transl.list")
  )


  ### PULL QUESTIONNAIRE ------------------------------------------------------
  # THOSE SHEETS WHICH WILL/SHOULD CONTAIN TRANSLATONS
  wsheets <- suppressMessages(get_odk_wsheet(
    gs = gs,
    sheets = sheets
  ))

  # TAKE TRANSLATION SHEET
  transsheet <- trans.list[[translation.to.add]]


  ## ADD TRANSLATION BY SHEET --------------------------------------------------------
  for (sheet in sheets) {

    # TAKE THAT QUESTIONNAIRE SHEET
    qx.sheet <- wsheets[[names(wsheets)[grepl(sheet, names(wsheets))]]]
    # GET ROW IDENTIFIER IN QUESTIONNAIRE SHEET
    qx.sheet[, rowid := 1:.N]
    # IDENTIFY FOR WHICH TYPE COLUMNS IT SHALL BE ADDED. BY DEFAULT FOR ALL TYPES (THAT ARE IN THE SHEET)
    if (is.null(types)) type.cols <- c(unique(transsheet$Type)[unique(transsheet$Type) %in% names(qx.sheet)]) else type.cols <- types

    # GO THROUGH ALL COLS HERE
    for (col in type.cols) {

      # NAME OF COLUMN
      trans.col <- paste0(gsub("\\s", "_", col), "::", translation.to.add)
      # IDENTIFY THE COLUMN IN WHICH TRANSLATION WILL BE WRITTEN. IF EXISTS, TAKE THAT INDEX
      # IF NOT, LAST COLUMN +1. BUT HERE SIMPLY NCOL AS WE ADDED ONE COL
      if (trans.col %in% names(qx.sheet)) col.start <- which(names(qx.sheet) == trans.col)
      if (!trans.col %in% names(qx.sheet)) col.start <- ncol(qx.sheet)

      # CREATE MERGE.VAR
      qx.sheet[, value.unique := stringr::str_to_lower(stringr::str_remove_all(get(col), " "))]

      # ADD TRANSLATION TO DT
      qx.sheet <- merge(
        #The current QX Sheet
        qx.sheet,
        #The translation sheet, subset by status user supplied
        transsheet[Status %chin% c("DeepL", "reviewed", "translated"), .(value.unique, Translation)]
                        , by = "value.unique", all.x = T)
      #EITHER CREATE OR UPDATE TRANSLATION COL
      qx.sheet[,c(trans.col):=Translation][,"Translation":=NULL]


      #REMOVE AGAIN SO OUR COUNT FOR COLSTART CONTINUES TO WORK
      qx.sheet[,"value.unique":=NULL]
      # GET IN ORDER
      setorder(qx.sheet, rowid)

      # WRITE TO QUESTIONNAIRE FILE
      write_trans_tsheet(
        qx.sheet[, .SD, .SDcols = trans.col],
        gs = gs,
        sheet = sheet,
        range = googlesheets4::cell_cols(col.start),
        col_names = TRUE
      )
    }
  }
}
