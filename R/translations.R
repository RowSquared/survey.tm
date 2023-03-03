#' Read Translation Sheets from Master Worksheet
#'
#' @param gs Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#' @param sheets Sheets in gs to read. Should be ISO languages. By default reads all sheets found in `gs`
#'
#' @return list of sheets
#' @export
#'
get_translations <- function(gs = "",
                             sheets = NULL) {

  # Check Sheets
  sheets.gs <- googlesheets4::gs4_get(gs)$sheets$name
  # Check if user supplied sheet is actually in the sheets
  if (!is.null(sheets)) {
    assertthat::assert_that(all(sheets %in% sheets.gs),
      msg =
        paste(paste(sheets[!sheets %in% sheets.gs], sep = ","), "is not a sheet in Google Sheet")
    )
  }

  # Define sheets. By default all sheets found in google sheet
  if (is.null(sheets)) sheets <- sheets.gs

  list <- purrr::map(
    .x = sheets,
    .f = ~ data.table::as.data.table(
      googlesheets4::read_sheet(gs,
        sheet = .x,
        range = "A:G",
        col_types = "ccccccc"
      )
    )
  )
  list <- setNames(list, c(sheets))

  return(list)
}



#' Update translation for one element in list
#'
#' @inheritParams  update_translation
#' @noRd
update_translation_lelement <- function(curr.trans,
                                        new.items) {

  # First Scenarion: Text Items exist in current Translation sheet and is found again in Master Questionnaire
  dt1 <- curr.trans[as.character(value.unique) %chin% new.items$value.unique]

  # Second Scenario: New items in Master Questionnaire not found yet in Translation Sheet
  dt2 <- new.items[
    !value.unique %chin% as.character(curr.trans$value.unique),
    .(value.unique,
      `Instrument(s)` = instrt,
      Type = type,
      Text_Item = value
    )
  ]

  # Bind to one
  dt <- rbindlist(list(
    dt1, dt2
  ), fill = TRUE)

  # Set Status to "to translate" if NA
  dt[is.na(Translation), Status := "to translate"]

  #Get in current sequential order, using the Master Questionnaire as reference
  dt <- merge(dt,new.items[,.(value.unique,seq.id)],by="value.unique",all.x=T)
  setorder(dt,seq.id)
  dt[,"seq.id":=NULL]

  return(dt)
}


#' Compares current questionnaire file against list of existing translations
#'
#' Removes any text item that no longer is part of the questionnaire(s).
#' Adds any text item that was not part of translation before
#'
#' @param curr.trans List of translations as returned by [get_translations()]
#' @param new.items Data table of questionnaire text items returned by either [parse_odk_titems()] or [parse_sursol_titems()]
#'
#' @return List of updated translations
#'
#' @export
#'
update_translation <- function(curr.trans = list(),
                               new.items = data.table()) {
  assertthat::assert_that(is.list(curr.trans))
  assertthat::assert_that(is.data.table(new.items))

  # Identify languages in current list of translations
  languages <- names(curr.trans)

  # Go through all sheets of current translation and compare against master
  updated.trans.sheets <- purrr::map(
    .x = languages,
    .f = ~ update_translation_lelement(
      curr.trans = curr.trans[[.x]],
      new.items = new.items
    )
  )
  updated.trans.sheets <- setNames(updated.trans.sheets, c(languages))

  return(updated.trans.sheets)
}






#
#' Write a data table to Translation Sheet
#' Simple wrapper for [googlesheets4::range_write()]
#'
#' @param dt Data table to be written. Usually an element within list of translations returned by `get_translations()` or `update_translation()`
#' @param gs Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#' @param sheet Name of sheet in `gs`. Usually name of translation which is name of element in list of translations.
#' @param range Cell Range to write
#' @param col_names Logical, indicates whether to send the column names of data
#'
#' @export
#'
write_trans_tsheet <- function(dt,
                               gs = "",
                               sheet = "",
                               range = "A2:G",
                               col_names = FALSE) {

  googlesheets4::range_write(
    ss = gs,
    data = dt,
    sheet = sheet,
    range = range,
    col_names = col_names,
    reformat = FALSE
  )
}
