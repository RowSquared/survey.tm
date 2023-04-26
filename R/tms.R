#FUNCTIONS TO WORK WITH THE TRANSLATION MASTER SHEET

#' Pull 'Translation Master' Google Sheet
#'
#' A simple wrapper for \code{\link[googlesheets4]{read_sheet}}.
#' Retrieves all data from the 'Translation Master' Google Sheet.
#'
#' @param ss Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#' @param sheets Sheets in ss to read. Should be ISO language names. By default reads all sheets found in `ss`
#'
#' @importFrom googlesheets4 gs4_get read_sheet
#' @importFrom data.table as.data.table
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#'
#' @return A list object where each element is one translation sheet
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_tms_data(ss = "GOOGLE-SHEET-IDENTIFIER")
#' }

get_tms_data <- function(ss = "",
                             sheets = NULL) {

  # Check Sheets
  sheets.ss <- googlesheets4::gs4_get(ss)$sheets$name

  # Check if user supplied sheet is actually in the sheets
  if (!is.null(sheets)) {
    assertthat::assert_that(all(sheets %in% sheets.ss),
      msg =
        paste(paste(sheets[!sheets %in% sheets.ss], sep = ","), "is not a sheet in the 'Translation Master' Google Sheet")
    )
  }

  # Define sheets. By default all sheets found in google sheet. Read all as character
  if (is.null(sheets)) sheets <- sheets.ss

  tms.list <- purrr::map(
    .x = sheets,
    .f = ~ data.table::as.data.table(
      googlesheets4::read_sheet(ss,
        sheet = .x,
        range = "A:G",
        col_types = "ccccccc"
      )
    )
  )
  tms.list <- setNames(tms.list, c(sheets))

  return(tms.list)
}



#
#' Write a data table to Translation Sheet
#' Simple wrapper for [googlesheets4::range_write()]
#'
#' @param dt Data table to be written. Usually an element within list of translations returned by `get_tms_data()` or `update_translation()`
#' @param ss Identifier of a Google Sheet, see [googlesheets4::sheets_id()]
#' @param sheet Name of sheet in `ss`. Usually name of translation which is name of element in list of translations.
#' @param range Cell Range to write
#' @param col_names Logical, indicates whether to send the column names of data
#'
#' @export
#'
write_trans_tsheet <- function(dt,
                               ss = "",
                               sheet = "",
                               range = "A2:G",
                               col_names = FALSE) {

  googlesheets4::range_write(
    ss = ss,
    data = dt,
    sheet = sheet,
    range = range,
    col_names = col_names,
    reformat = FALSE
  )
}
