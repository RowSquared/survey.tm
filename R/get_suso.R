
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

get_suso_tfile <- function(questionnaire = "",
                           user = "",
                           password = "",
                           sheets = NULL) {

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
  # Check response
  if (request$status_code != 200) {
    stop(
      paste0(
        "Survey Solutions returned status code ", request$status_code, " when trying to download the questionnaire.\nCheck the User, Password and questionnaire paramaters provided."
      )
    )
  }

  # WRITE TO TEMPFILE
  tmp.file <- tempfile(fileext = ".xlsx")
  writeBin(request$content, tmp.file)

  # IDENTIFY ALL SHEETS
  sheets.file <- readxl::excel_sheets(tmp.file)
  # Check if user supplied sheet is actually in the sheets. If not provide warning
  if (!is.null(sheets) & !all(sheets %in% sheets.file)) {
    warning(paste(
      paste(sheets[!sheets %in% sheets.file], collapse = ", "),
      "are no sheet(s) in Designer Template file", questionnaire
    ))
  }

  # READ EACH SHEET INTO LIST AND SET NAMES
  sheet.list <- purrr::map(
    .x = sheets.file,
    .f = ~ as.data.table(readxl::read_excel(
      path = tmp.file,
      sheet = .x,
      col_types = c("text")
    ))
  )

  sheet.list <- setNames(sheet.list, c(sheets.file))

  # Place the Translation into list, to allow easier handling of multiple questionnaires
  list.final <- list(sheet.list)
  # Name as Questionnaire Title
  list.final <- setNames(list.final, sheet.list[["Translations"]][1, `Original text`])

  return(list.final)
}



#' Retrieve 'Source Questionnaire' templates from SurSol Designer
#'
#' This function retrieves the translation files for (multiple) questionnaires from the \href{https://designer.mysurvey.solutions}{Survey Solutions Designer}.
#'
#' @param questionnaires A character vector of questionnaire IDs within Survey Solutions Designer.
#' @param user A character string representing the Survey Solutions Designer username.
#' @param password A character string representing the Survey Solutions Designer password.
#' @param sheets A character vector of sheet names from the translation files to read. By default, all sheets are read.
#'
#' @import data.table
#' @importFrom httr GET authenticate
#' @importFrom readxl excel_sheets
#' @importFrom purrr map list_flatten flatten
#'
#' @return Returns a nested list, called 'Source Questionnaire(s)'. The top-level elements represent questionnaires, and each contains a list of data.tables corresponding to the sheets within the respective questionnaire.
#' @export
#'
#'@examples
#' \dontrun{
#' # Define your Survey Solutions Designer credentials
#'
#' # Define the questionnaire IDs you want to retrieve translations for
#' questionnaires <- c("12345678901234567890123456789012", "23456789012345678901234567890123")
#'
#' # Retrieve the 'Source Questionnaire' template files
#' suso_trans_templates <- get_suso_tfiles(
#'   questionnaires = questionnaires,
#'   user = "your_email@example.com",
#'   password =  "your_password",
#'   sheets = c("Translations", "@@_myreusable_category")
#' )
#'
#' # Access the 'Source Questionnaire' of the first questionnaire template
#' translations_first_questionnaire <- suso_trans_templates[["NAME-OF-QUESTIONNAIRE"]]
#'
#' # Access the "Translations" sheet for the first questionnaire
#' translations_sheet_first_questionnaire <- translations_first_questionnaire[["Translations"]]
#'
#'}
#'
get_suso_tfiles <- function(questionnaires = "",
                            user = "",
                            password = "",
                            sheets = NULL) {

  # CHECK INPUT
  assertthat::assert_that(all(nchar(questionnaires) == 32), msg = "Questionnaire IDs must be 32 alpha-numeric identifier")
  assertthat::assert_that(nchar(user) > 0, msg = "'user' must not be an empty string.")
  assertthat::assert_that(grepl("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", user), msg = "The 'user' parameter must be a valid email address.")
  assertthat::assert_that(nchar(password) > 0, msg = "'password' must not be an empty string.")

  if (!is.null(sheets)) {
    assertthat::assert_that(is.character(sheets), msg = "'sheets' must be a character vector.")
    assertthat::assert_that(any(sheets == "Translations"), msg = "The 'sheets' parameter must contain 'Translations' or be NULL.")
  }


  # Retrieve Translations & Flatten immediately
  translations_list <- purrr::map(
    .x = questionnaires,
    .f = ~ get_suso_tfile(.x,
      user = user,
      password = password,
      sheets = sheets
    )
  )
  translations_list <- purrr::list_flatten(translations_list)



  return(translations_list)
}
