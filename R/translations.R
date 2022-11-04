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
  # TODO: IF SHeets Defined, check if actually exists
  # Define sheets. By default all sheets found in google sheet
  if (is.null(sheets)) sheets <- googlesheets4::gs4_get(gs)$sheets$name

  list <- purrr::map(
    .x = sheets,
    .f = ~ data.table::as.data.table(
      googlesheets4::read_sheet(gs,
        sheet = .x
      )
    )
  )
  list <- setNames(list, c(sheets))

  return(list)
}



#' Update translation for one element in list
#'
#' @inheritParams  update_translation
update_translation_lement <- function(curr.trans,
                                      new.items) {
  dt <- rbindlist(list(
    # TAKE CURRENT TRANSLATION SHEET ITEMS AND COMPARE AGAINST MASTER TRANSLATION ITEMS.
    curr.trans[as.character(value.unique) %chin% new.items$value.unique],
    # FROM NEW MASTER LIST ALL THOSE NOT YET IN TRANSLATION SHEET
    new.items[
      !value.unique %chin% as.character(curr.trans$value.unique),
      .(value.unique,
        `Instrument(s)` = instrt,
        Type = type,
        Text_Item = value
      )
    ]
  ), fill = TRUE)

  return(dt)
}


#' Compares current questionnaire file against list of existing translations
#'
#' Removes any text item that no longer is part of the questionnaire(s).
#' Adds any text item that was not part of translation before
#'
#' @param curr.trans List of translations as returned by [get_translations()]
#' @param new.items Data table of questionnaire text items returned by either [parse_odk_titems()] or [get_sursol_titems()]
#'
#' @return List of updated translations
#'
#' @export
#'
update_translation <- function(curr.trans = list(),
                               new.items = data.table()) {
  #TODO: NEW ITEMS ARE NOW AT END OF DATA TABLE. CHANGE TO SEQUENTIAL ORDER?
  assertthat::assert_that(is.list(curr.trans))
  assertthat::assert_that(is.data.table(new.items))

  # Identify translations in current list of translations
  translations <- names(curr.trans)
  updated.trans.sheets <- purrr::map(
    .x = translations,
    .f = ~ update_translation_lement(
      curr.trans = curr.trans[[.x]],
      new.items = new.items
    )
  )
  updated.trans.sheets <- setNames(updated.trans.sheets, c(translations))

  return(updated.trans.sheets)
}






#
#' Write a data table to Translation Sheet
#' Simple wrapper for [googlesheets4::range_write()]
#'
#' @param dt Data table to be written
#' @param gs Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#' @param sheet Name of sheet in `gs`
#' @param range Cell Range to write
#' @param col_names Logical, indicates whether to send the column names of data
#'
#' @export
#'
write_trans_tsheet <- function(dt,
                         gs="",
                         sheet="",
                         range="A2:G",
                         col_names=FALSE) {

  googlesheets4::range_write(
    ss=gs,
    data=dt,
    sheet = sheet,
    range = range,
    col_names = col_names,
    reformat = FALSE
  )
}
