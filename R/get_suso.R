
#' Retrieve translation file from SurSol Designer
#'
#' @inheritParams get_suso_tfiles
#' @param questionnaire Questionnaire id within Survey Solutions Designer
#'
#' @seealso \code{\link{get_suso_tfiles}}
#'
#' @import data.table
#' @importFrom httr GET authenticate
#' @importFrom readxl excel_sheets
#'
#' @noRd
#'
#' @return List of sheets in translation file

get_suso_tfile <- function(
    questionnaire="",
    user="",
    password="",
    sheets=NULL

) {
  #TODO: Sheets must contain "Translations" or be NULL.

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
                       sheet = .x,
                       col_types = c("text")
                     )))

  list <- setNames(list, c(sheets.file))

  #Place the Translation into list, to allow easier handling of multiple instruments
  list.final <- list(list)
  #Name as Questionnaire Title
  list.final <- setNames(list.final,list[["Translations"]][1,`Original text`] )

  return(list.final)

}



#' Retrieve mutiple translation files from SurSol Designer
#'
#' Wrapper for \code{\link{get_suso_tfile}} to retrieve multiple translation files
#'
#' @param questionnaires Vector of Questionnaire id's within Survey Solutions Designer
#' @param user SuSo Designer User Name
#' @param password SuSo Designer Password
#' @param sheets Sheets from Translation to read. Default all
#'
#' @import data.table
#' @importFrom httr GET authenticate
#' @importFrom readxl excel_sheets

#' @return List of translation file with list of sheets
#' @export
#'
get_suso_tfiles <- function(
    questionnaires="",
    user="",
    password="",
    sheets=NULL

) {

  #CHECK INPUT
  assertthat::assert_that(all(nchar(questionnaires)==32),msg="Questionnaire IDs must be 32 alpha-numeric identifier")


  #Retrieve Translations & Flatten immediately
  list <- purrr::map(.x=questionnaires,
             .f=~get_suso_tfile(.x,
                                user=user,
                                password=password,
                                sheets=sheets)) %>% purrr::flatten()



  return(list)

}


