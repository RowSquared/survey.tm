#' Add provided user's Translation to a Survey Solutions Translation sheet
#'
#' @param sheet Sheet in `questionnaire.list` to which translation should be added
#' @param data.table of translation to be added to Questionnaire
#' @inheritParams create_suso_file
#'
#' @return data table of `sheet` with Translation added
#'
add_suso_sheet <- function(questionnaire.list = "",
                           sheet = "",
                           translation.dt = "",
                           pattern = NULL) {

  # Get Sheet from List
  qx.tsheet <- questionnaire.list[[sheet]]

  # Get names for later
  names.sheet <- names(qx.tsheet)

  # GET ROW IDENTIFIER IN QUESTIONNAIRE SHEET
  qx.tsheet[, rowid := 1:.N]

  # Remove Coding
  # Get col of code itself
  if (!is.null(pattern)) qx.tsheet[grepl(pattern, `Original text`), coding := stringr::str_extract(`Original text`, pattern)]
  # Remove Col from Original Text
  if (!is.null(pattern)) qx.tsheet[grepl(pattern, `Original text`), `Original text` := stringr::str_remove(`Original text`, pattern)]

  # CREATE MERGE.VAR
  create.unique.var(qx.tsheet, col = "Original text")

  # ADD TRANSLATION TO DT
  qx.tsheet <- merge(
    # The current QX Sheet, without Translation column
    qx.tsheet[, "Translation" := NULL],
    # The translation sheet, subset by status user supplied
    translation.dt[, .(value.unique, Translation)],
    by = "value.unique", all.x = T
  )


  # Check for duplicate Option Title
  if (any(duplicated(qx.tsheet, by = c("Entity Id", "Type", "Translation")))) {
    # Identify duplicates
    qx.tsheet[Type == "OptionTitle", dupl := .N > 1,
      by = c("Entity Id", "Type", "Translation")
    ]
    # Print them to console
    message("Attention, categories title must be unique.")
    message(paste(
      qx.tsheet[dupl == T, .(Translation = Translation[c(1)]), by = "Original text"][, .(text = paste0("\t", paste(paste(`Original text`, collapse = ", "), Translation, sep = ": "))), by = "Translation"]$text,
      collapse = "\n"
    ))

    message("Translation(s) for these text items will not be added to upload file")
    qx.tsheet[dupl == T, Translation := NA][, "dupl" := NULL]
  }

  # Get back Coding to Original Text and Translation
  # Get col of code itself
  if (!is.null(pattern)) {
    qx.tsheet[!is.na(coding), `:=`
    (
      `Original text` = paste0(coding, `Original text`),
      Translation = paste0(coding, Translation)
    )][, "coding" := NULL]
  }

  # Correct (Col) Order
  setcolorder(qx.tsheet, c("Entity Id", "Variable", "Type", "Index", "Original text", "Translation"))
  setorder(qx.tsheet, rowid)
  qx.tsheet[, c("rowid", "value.unique") := NULL]


  # Return
  return(qx.tsheet)
}






#' Create Survey Solutions Translation file for upload
#'
#' Based on list of translations and Survey Solutions Questionnaire ID, this function will create a .xlsx file on your hard-drive which can be used to be uploaded to the Survey Solutions Designer.
#'
#' @param trans.list List. Translations created by \code{\link{get_translations}} or \code{\link{update_translation}}
#' @param translation Character. Which element of `trans.list` to be used
#' @param questionnaire.list List of Questionnaire Translation returned by \code{\link{get_suso_tfiles}}
#' @param path Character. Writable file path where Translation File should be stored at, including file name and extension
#' @param sheets Character vector. For which sheets of questionnaire template file translation will be added. Default all sheets that are found in template file
#' @param pattern Regular expression that matches question coding. Should be specified if `remove_coding()` was used to process translation file
#' @param statuses Character vector. Which items from Translation Google Sheet should be merged?
#'
#' @export
#'
create_suso_file <- function(trans.list = list(),
                             translation = stop("'translation' must be specified"),
                             questionnaire.list=list(),
                             path = stop("'path' must be specified"),
                             sheets = NULL,
                             statuses=c("Machine","reviewed","translated"),
                             pattern = NULL) {

  # CHECK INPUT -------------------------------------------------------------
  # TODO: CHECK IF ALL ITEMS HAVE NOW TRANSLATION. IF NOT, MAYBE FLAG THAT PATTERN IS NOT SUPPLIED?

  # Translation list and Translation
  assertthat::assert_that(translation %in% names(trans.list),
    msg = paste(translation, "is not an element in transl.list")
  )
  # Translation
  assertthat::assert_that(length(translation) == 1, msg = "'translation' must be character vector of length 1")

  #Questionnaire List
  # Check that all elements are named & are data.tables
  assertthat::assert_that(all(names(questionnaire.list) != ""), msg="All elements in questionnaire.list must be named.")
  assertthat::assert_that(suppressWarnings(all(lapply(questionnaire.list,is.data.table))),
                          msg="All elements in questionnaire.list must be data.table. Did you supply a list that contains multiple questionnaires?")



   # Path
  assertthat::assert_that(grepl(".xlsx$", path), msg = "'path' must have .xlsx file extension")
  assertthat::assert_that(dir.exists(dirname(path)), msg = paste(path, "does not exist"))

  # Sheets
  #Check if user supplied sheet is actually in the sheets.
  if (!is.null(sheets)) {
    #Get the sheets in translation list
    sheets.questionnaire  <- names(questionnaire.list)

    #If "Translations" was not specified add it
    if (!"Translations" %in% sheets) sheets <- c(sheets,"Translations")

    #Get which ones are not found
    sheets.not.found <- sheets[!sheets %in% sheets.questionnaire]
    assertthat::assert_that(length(sheets.not.found)==0, msg=paste(paste(sheets.not.found,collapse=", "),"are sheets not found in questionnaire.list"))

  }
  #If Sheet is null, take all sheets found in Questionnaire Master
  if (is.null(sheets)) sheets <- names(questionnaire.list)

  #Statuses
  #Create a dt of our from user input
  translation.dt <-  trans.list[[translation]]
  #Check statuses that are in fact in the dt
  statuses.in.translation <- unique(translation.dt$Status)
  if (!all(statuses %in% statuses.in.translation)) warning(
    paste("Statuses",
          paste(statuses[!statuses %in% statuses.in.translation],collapse=", "), "are currently not present in Translation Sheet")
  )





  #Get subset of translation sheet
  translation.dt <- translation.dt[Status %in% statuses]
  #If there is no translation in our preferred statuses, return simply the questionnaire
  if (nrow(translation.dt)==0) {
    message("No translation found that is in Status as supplied in 'statuses'. Empty ")
    message("No excel file generated")
    return()
  }

  # Add Translation by Sheet. Results in Translation Workbook list
  workbook <- purrr::map(
    .x = sheets,
    .f = ~ add_suso_sheet(
      questionnaire.list = questionnaire.list,
      sheet = .x,
      translation.dt = translation.dt,
      pattern = pattern
    )
  )


  workbook <- setNames(workbook, c(sheets))

  # Write the Workbook!
  writexl::write_xlsx(
    workbook,
    path = path
  )
}
