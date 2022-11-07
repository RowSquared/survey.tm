#' Add provided user's Translation to a Survey Solutions Translation sheet
#'
#' @param file Excel file (SuSo Translation File)
#' @param sheet Sheet in `file` to which translation should be added
#'
#' @return data table of `sheet` with Translation added
#'
add_suso_sheet <- function(file = "",
                           sheet = "",
                           translation.file = "",
                           pattern = NULL) {


  # Read Sheet
  file.sheet <- data.table::as.data.table(readxl::read_excel(
    path = file,
    sheet = sheet
  ))
  # Get names for later
  names.sheet <- names(file.sheet)

  # GET ROW IDENTIFIER IN QUESTIONNAIRE SHEET
  file.sheet[, rowid := 1:.N]

  # Remove Coding
  # Get col of code itself
  if (!is.null(pattern)) file.sheet[grepl(pattern, `Original text`), coding := stringr::str_extract(`Original text`, pattern)]
  # Remove Col from Original Text
  if (!is.null(pattern)) file.sheet[grepl(pattern, `Original text`), `Original text` := stringr::str_remove(`Original text`, pattern)]

  # CREATE MERGE.VAR
  create.unique.var(file.sheet, col = "Original text")

  # ADD TRANSLATION TO DT
  file.sheet <- merge(
    # The current QX Sheet, without Translation column
    file.sheet[, "Translation" := NULL],
    # The translation sheet, subset by status user supplied
    translation.file[Status %chin% unique(translation.file$Status), .(value.unique, Translation)],
    by = "value.unique", all.x = T
  )


  # Check for duplicate Option Title
  if (any(duplicated(file.sheet, by = c("Entity Id", "Type", "Translation")))) {
    # Identify duplicates
    file.sheet[Type == "OptionTitle", dupl := .N > 1,
      by = c("Entity Id", "Type", "Translation")
    ]
    # Print them to console
    message("Attention, categories title must be unique.")
    message(paste(
      file.sheet[dupl == T, .(Translation = Translation[c(1)]), by = "Original text"][, .(text = paste0("\t", paste(paste(`Original text`, collapse = ", "), Translation, sep = ": "))), by = "Translation"]$text,
      collapse = "\n"
    ))

    message("Translation(s) for these text items will not be added to upload file")
    file.sheet[dupl == T, Translation := NA][, "dupl" := NULL]
  }

  # Get back Coding to Original Text and Translation
  # Get col of code itself
  if (!is.null(pattern)) {
    file.sheet[!is.na(coding), `:=`
    (
      `Original text` = paste0(coding, `Original text`),
      Translation = paste0(coding, Translation)
    )][, "coding" := NULL]
  }

  # Correct (Col) Order
  setcolorder(file.sheet, c("Entity Id", "Variable", "Type", "Index", "Original text", "Translation"))
  setorder(file.sheet, rowid)
  file.sheet[, c("rowid", "value.unique") := NULL]
  # Return
  return(file.sheet)
}






#' Create Survey Solutions Translation file for upload
#'
#' Based on list of translations and Survey Solutions Questionnaire ID, this function will create a .xlsx file on your hard-drive which can be used to be uploaded to the Survey Solutions Designer.
#'
#' @param trans.list List. Translations created by `get_translations()` or `update_translation()`
#' @param translation Character. Which element of `trans.list` to be used
#' @param questionnaire Character. Questionnaire ID within Survey Solutions Designer. Usually 32 character id. Can be found in URL when opening the questionnaire: `https://designer.mysurvey.solutions/questionnaire/details/XXXXXXXXXX`
#' @param user Character. SuSo Designer User Name
#' @param password Character. SuSo Designer Password
#' @param path Character. Writable file path where Translation File should be stored at, including file name and extension
#' @param sheets Character vector. For which sheets of questionnaire template file translation will be added. Default all sheets that are found in template file
#' @param pattern Regular expression that matches question coding. Should be specified if `remove_coding()` was used to process translation file
#'
#' @export
#'
create_suso_file <- function(trans.list = list(),
                             translation = stop("'translation' must be specified"),
                             questionnaire = stop("'questionnaire' must be specified"),
                             user = stop("'user' must be specified"),
                             password = stop("'password' must be specified"),
                             path = stop("'path' must be specified"),
                             sheets = NULL,
                             pattern = NULL) {



  # CHECK INPUT -------------------------------------------------------------


  # TODO: questionnaire Must be length 1
  # TODO: CHECK IF ALL ITEMS HAVE NOW TRANSLATION. IF NOT, MAYBE FLAG THAT PATTERN IS NOT SUPPLIED?

  # Translation list and Translation
  assertthat::assert_that(translation %in% names(trans.list),
    msg = paste(translation, "is not an element in transl.list")
  )
  # Translation
  assertthat::assert_that(length(translation) == 1, msg = "'translation' must be character vector of length 1")

  # Path
  assertthat::assert_that(grepl(".xlsx$", path), msg = "'path' must have .xlsx file extension")
  assertthat::assert_that(dir.exists(dirname(path)), msg = paste(path, "does not exist"))


  # GET DESIGNER TEMPLATE FILE -------------------------------------------------------------

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
        "Survey Solutions returned status code ", request$status_code, " when trying to download the instrument.\nCheck the User, Password and questionnaire paramaters provided."
      )
    )
  }


  # WRITE TO TEMPFILE
  tmp.file <- tempfile(fileext = ".xlsx")
  writeBin(request$content, tmp.file)

  # IDENTIFY ALL SHEETS
  sheets.file <- readxl::excel_sheets(tmp.file)
  # Check if user supplied sheet is actually in the sheets
  if (!is.null(sheets)) {
    assertthat::assert_that(all(sheets %in% sheets.file),
      msg =
        paste(paste(sheets[!sheets %in% sheets.file], sep = ","), "is not a sheet in Designer Template file", questionnaire)
    )
  }

  # If sheet not supplied, take all sheets
  if (is.null(sheets)) sheets <- sheets.file

  # Add Translation by Sheet. Results in Translation Workbook list
  workbook <- purrr::map(
    .x = sheets,
    .f = ~ add_suso_sheet(
      file = tmp.file,
      translation.file = trans.list[[translation]],
      sheet = .x,
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
