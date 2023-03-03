#' Get unique Text Items of one questionnaire
#'
#' @param list that contains sheets of one instrument
#' @param sheets Sheets from Translation to read. Default all
#' @param types Which type of text items to keep
#' @import data.table
#' @noRd
parse_sursol_titems.by.qx <- function(
    list,
    instrument=stop("instrument needs to be defined"),
    sheets = NULL,
    types = c(
      "Title", "Instruction", "OptionTitle", "ValidationMessage",
      "SpecialValue","FixedRosterTitle"
    )) {


  #Keep only subset of sheets
  if (!is.null(sheets)) {
    sheets.list <- sheets[sheets %in% names(list)]
    list <- list[c(sheets.list)]

  }
  # READ ALL SHEETS INTO ONE DT- ONLY COLS OF INTEREST

  dt <- rbindlist(
    lapply(list, \(sheet) {
      sheet[
        Type %chin% types | is.na(Type),
        .(
          type = Type,
          value = `Original text`
        )
      ]
    })
  )

  # GET ROW IDENTIFIER
  dt[, seq.id := 1:.N]

  #Assign instrument identifier
  dt[,instrt:=instrument]

  return(dt)
}





#' Get (unique) Text Items from list of Survey Solutions translation(s) in rectangular format
#'
#' @param translation Named list where each element contains the translation sheets of each instrument
#' @param sheets Sheets from Translation to read. Default all
#' @param types Which type of text items to keep
#' @param collapse Boolean. If TRUE, only 'unique' text items will be returned.
#'
#' @import data.table
#' @importFrom httr GET authenticate
#' @importFrom readxl excel_sheets
#' @import data.table

#' @return Data table of unique text items in instrument(s)
#' @export
#'
parse_sursol_titems <- function(translation,
                                sheets = NULL,
                                types = c(
                                  "Title", "Instruction", "OptionTitle", "ValidationMessage",
                                  "SpecialValue","FixedRosterTitle"
                                ),
                                collapse=TRUE) {
  #TODO: Assert that Translations is in sheets or simply add it?


  #Check input
  types <- match.arg(types,several.ok = T)

  assertthat::assert_that(all(!is.null(names(translation))),
                          msg = "'translation' is not named list. Use Instrument Name for each element")

  # Check  Sheets
  if (!is.null(sheets) ) {

  # Check if sheets is a character vector
  if  (!is.character(sheets)) stop("sheets must be NULL or a character vector")

  #Check if user supplied sheet is actually in the sheets. If not provide warning
  #Get the sheets in translation list
  sheets.list <- unlist(lapply(translation, names))
  #Get which ones are not found
  sheets.not.found <- sheets[!sapply(sheets, function(name) all(sapply(translation, function(inner_list) name %in% names(inner_list))))]


  if (length(sheets.not.found)>0 ) warning(paste(paste(sheets.not.found,collapse=", "),
                                                                       "are sheet(s) not found in all Translation Files"))

  #If "Translation" not within sheets, add it
  if (!"Translations" %in% sheets) sheets <- c(sheets,"Translations")
  }

  #Go through all instruments in translation list and bind in one
  dt <- purrr::map_df(.x=names(translation),
                      .f=~parse_sursol_titems.by.qx(list=translation[[.x]],
                                                    instrument=.x,
                                                    sheets=sheets,
                                                    types = types)
  )
  #Convenience, convert to data.table
  dt <- as.data.table(dt)


  # Cleanup Text item to catch more duplicate text items
  create.unique.var(dt)

  #Collapse if specified
  if (collapse) dt <- collapse_titems(dt)

  return(dt)
}
