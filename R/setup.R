#' Creates new 'Translation Master Sheet'
#'
#' Creates a new Google Sheet (based on the \href{https://docs.google.com/spreadsheets/d/1xLHEDm5bgtv4IHHCbfyMf_sdNsdzz_AUcRrnpb2fXTw}{translation template}) in your Google user account. This new sheet will serve as the main 'Translation Master Sheet' in subsequent workflow.
#'
#' @param ssheet_name Character. The name that will be assigned to the Google Sheet.
#' @param lang_names Character Vector. A vector of \href{https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes}{ISO 639} language names for which translation will be needed. Each language becomes one sheet/tab.
#'
#' @return List of metadata on the newly created sheet.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a new translation master sheet with two languages (German and French)
#' new_sheet <- setup_tsheet(
#'   ssheet_name = "Project X: Translation Sheet",
#'   lang_names = c("German", "French")
#' )
#'
#' # The returned 'new_sheet' object contains metadata about the created sheet
#' new_sheet$spreadsheet_url # Access the URL of the created sheet
#'
#' # Print all metadata
#' new_sheet
#' }
#'
setup_tsheet <- function(ssheet_name = stop("'ssheet_name' must be specified"),
                         lang_names = stop("'lang_names' must be specified")) {

  # Additional check for input parameters
  assertthat::assert_that(
    is.character(ssheet_name),
    msg = "'ssheet_name' must be of type character."
  )
  assertthat::assert_that(
    is.character(lang_names),
    msg = "'lang_names' must be a character vector."
  )

  # CHECK Google File
  message(paste0("Checking if '", ssheet_name, "' does exist already."))

  # CHECK IF GOOGLE WORKSHEET NAME ALREADY EXISTS.
  any.exist.sheets <- invisible(googlesheets4::gs4_find(order_by = "createdTime desc", pattern = ssheet_name))
  # IF SO, RETURN ERROR OR ASK IF IT SHALL BE DE DELETED WIT USER INPUT
  n.rows <- nrow(any.exist.sheets)
  if (n.rows > 0) {
    stop(paste("There are", n.rows, "spreadsheet(s) with the name", ssheet_name, "already. \n  Please use a different name or delete the existing Google Sheet(s)"))
  }

  # Language specified
  # Check if specified languages are valid ISO 639-1 codes
  for (lan in lang_names) {
    assertthat::assert_that(
      nrow(languages[grepl(lan, ISO.language.name)]) == 1,
      msg = paste(lan, "is not a ISO 639 language. Check\nhttps://en.wikipedia.org/wiki/List_of_ISO_639-1_codes\nCase sensitive!")
    )
  }



  # CREATE NEW SHEET.
  new.ssheet <- googlesheets4::gs4_create(name = ssheet_name, sheets = "placeholder")

  # NOW FOR EACH LANGUAGE SPECIFIED, COPY SHEET
  purrr::walk(
    .x = lang_names,
    .f = ~ invisible(googlesheets4::sheet_copy(
      from_ss = "1xLHEDm5bgtv4IHHCbfyMf_sdNsdzz_AUcRrnpb2fXTw",
      to_ss = new.ssheet,
      to_sheet = .x
    ))
  )

  # Remove placeholder
  invisible(googlesheets4::sheet_delete(new.ssheet, sheet = "placeholder"))

  # Print Result
  sheet_meta <- googlesheets4::gs4_get(new.ssheet)
  message("Spreadsheet", ssheet_name, " successfully set up. You can access it at\n", sheet_meta$spreadsheet_url)

  # Return Metdata of Sheet if of use
  return(sheet_meta)
}
