#' Creates new google worksheet using translation template
#'
#' @param name.ssheet Character. Name that will be assigned to  Google Worksheet
#' @param translation.languages Character Vector. ISO 639-1 languages for which translation will be needed. Each language becomes one sheet
#'
#' @return List of metadata on new sheet
#' @export
#'
setup_tsheet <- function(
    name.ssheet=stop("'name.sshet' must be specified"),
    translation.languages=stop("'translation.languages' must be specified")
) {

  #CHECK INPUT

  #CHECK IF GOOGLE WORKSHEET NAME ALREADY EXISTS. IF SO, RETURN ERROR OR ASK IF IT SHALL BE DE DELETED WIT USER INPUT
  any.exist.sheets <-  invisible(googlesheets4::gs4_find(order_by = "createdTime desc",name.ssheet))
  n.rows <- nrow(any.exist.sheets)
  if (n.rows>0) {
    stop(paste("There are",n.rows,"spreadsheet(s) of name",name.ssheet,"already.\nPlease use a different name or delete the Google Sheet(s)"))
  }

  #Language specified
  for (lan in translation.languages) {
    assertthat::assert_that(
      nrow(languages[grepl(lan,ISO.language.name)])==1,
      msg=paste(lan, "is not a ISO 639 language. Check\nhttps://en.wikipedia.org/wiki/List_of_ISO_639-1_codes\nCase sensitive!" ))
  }


  #CREATE NEW SHEET.
  new.ssheet <- googlesheets4::gs4_create(name=name.ssheet,sheets = "placeholder")

  #NOW FOR EACH LANGUAGE SPECIFIED, COPY SHEET
  purrr::walk(
    .x=translation.languages,
    .f=~invisible(googlesheets4::sheet_copy(from_ss ="1xLHEDm5bgtv4IHHCbfyMf_sdNsdzz_AUcRrnpb2fXTw",
                   to_ss = new.ssheet,
                   to_sheet=.x))
  )

  #Remove placeholder
  invisible(googlesheets4::sheet_delete(new.ssheet,sheet="placeholder"))

  #Print Result
  sheet_meta <- googlesheets4::gs4_get(new.ssheet)
  message("Spreadsheet",name.ssheet," successfully set up. You can access it at\n",sheet_meta$spreadsheet_url)

  #Return Metdata of Sheet if of use
  return(sheet_meta)
}
