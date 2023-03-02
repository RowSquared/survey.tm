
#' Retrieve translation file from SurSol Designer
#'
#'
#' @param qx_id Questionnaire id within Survey Solutions Designer
#' @param user SuSo Designer User Name
#' @param password SuSo Designer Password
#' @param sheets Sheets from Translation to read. Default all
#'
#' @import data.table
#' @importFrom httr GET authenticate
#' @importFrom readxl excel_sheets

#' @return List of sheets in translation file
#' @export
get_suso_tfile <- function(
    questionnaire="",
    user="",
    password="",
    sheets=NULL

) {
  #TODO: CHECK IF 32 BUT WRONG ID.

  #CHECK INPUT
  assertthat::assert_that(all(nchar(questionnaire)==32),msg="Questionnaire IDs must be 32 alpha-numeric identifier")

  # BUILD URL
  url <- paste0("https://designer.mysurvey.solutions/translations/", questionnaire, "/template")

  # GET THE TRANSLATION FILE
  request <- httr::GET(
    url = url,
    httr::authenticate(
      user,
      password
    )
  )
  #Check response
  if (request$status_code!= 200) {
    stop(
      paste0(
        "Survey Solutions returned status code ",  request$status_code,  " when trying to download the instrument.\nCheck the User, Password and questionnaire paramaters provided.")
    )
  }

  # WRITE TO TEMPFILE
  tmp.file <- tempfile(fileext = ".xlsx")
  writeBin(request$content, tmp.file)

  # IDENTIFY ALL SHEETS
  sheets.file <- readxl::excel_sheets(tmp.file)
  # Check if user supplied sheet is actually in the sheets. If not provide warning
  if (!is.null(sheets) & !all(sheets %in% sheets.file) ) warning(paste(paste(sheets[!sheets %in% sheets.file],collapse=", "),
                                                                      "are no sheet(s) in Designer Template file",questionnaire))

  #READ INTO LIST AND SET NAMES
  list <- purrr::map(.x=sheets.file,
                     .f=~as.data.table(readxl::read_excel(
                       path = tmp.file,
                       sheet = .x
                     )))
  list <- setNames(list, c(sheets.file))

  return(list)

}


#' Get unique Text Items of questionnaire
#'
#' @param qxid Id of Questionnaire in SuSo Designer
#' @param user SuSo Designer User Name
#' @param password SuSo Designer Password
#' @param sheets Sheets from Translation to read. Default all
#' @param types Which type of text items to keep
#' @importFrom httr GET authenticate
#' @importFrom readxl excel_sheets
#' @import data.table
#' @noRd
get_sursol_titems_byqx <- function(qxid = NULL,
                                   user = "",
                                   password = "",
                                   sheets = NULL,
                                   types = c(
                                     "Title", "Instruction", "OptionTitle", "ValidationMessage",
                                     "SpecialValue","FixedRosterTitle"
                                   )) {


  #GET AND READ TRANSLATION FILE
  list <- get_suso_tfile(
    questionnaire=qxid,
    user=user,
    password=password,
    sheets=sheets)


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

  # CREATE UNIQUE VALUE
  create.unique.var(dt)

  # GET ROW IDENTIFIER
  dt[, seq.id := 1:.N]


  return(dt)
}





#' Get unique Text Items of questionnaire(s) from Survey Solutions
#'
#' @param questionnaires Named character vector of instruments to source. Elements must be id of questionnaire. Name is the Title of Questionnaire.
#' @param user SuSo Designer User Name
#' @param password SuSo Designer Password
#' @param sheets Sheets from Translation to read. Default all
#' @param types Which type of text items to keep
#'
#' @import data.table
#' @importFrom httr GET authenticate
#' @importFrom readxl excel_sheets
#' @import data.table

#' @return Data table of unique text items in instrument(s)
#' @export
#'
get_sursol_titems <- function(questionnaires = NULL,
                              user = "",
                              password = "",
                              sheets = NULL,
                              types = c(
                                "Title", "Instruction", "OptionTitle", "ValidationMessage",
                                "SpecialValue","FixedRosterTitle"
                              )) {

  #TODO: Sheets doesnt seem to work

  #Check input
  types <- match.arg(types,several.ok = T)

  assertthat::assert_that(is.char.named.vector(questionnaires),
                          msg = "questionnaires is not named character vector. Use Instrument Name for each element")

  assertthat::assert_that(all(nchar(questionnaires)==32),msg="Questionnaire IDs must be 32 alpha-numeric identifier")


  # GET ALL QUESTIONNAIRE ITEMS AND ALL SHEETS IN ONE DT
  dt <- rbindlist(lapply(questionnaires, \(x) {
    get_sursol_titems_byqx(
      qxid = x,
      user = user,
      password = password,
      sheets = sheets,
      types = types
    )[, instrt := names(questionnaires)[questionnaires %in% x]]
  }))


  dt <- collapse_titems(dt)

  return(dt)
}
