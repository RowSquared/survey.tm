#' Add Google API  translation to data table. I.e. for an element of list of translations in tms
#'
#' @param dt Data table of translations
#' @inheritParams  add_gl_translation
#' @noRd
#'
add_gl_translate_dt <- function(dt,
                                source_lang = "English",
                                target_lang = NULL,
                                API_key = NULL) {

  # Check input
  if (!requireNamespace("googleLanguageR", quietly = TRUE)) {
    stop(
      "Package \"googleLanguageR\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # TODO: ASSERT THAT ALL COLUMNS THAT ARE EXPECTED ARE IN dt
  # CHECK INPUT
  # Any translation actually missing?
  if (nrow(dt[is.na(Translation)]) == 0) {
    message(paste("dt has no row with missing 'Translation'. Google Translate API will not be queried for", target_lang))
    return(invisible(dt))
  }

  # Languages correctly specified?
  available.languages <- googleLanguageR::gl_translate_languages()
  assertthat::assert_that(source_lang %in% available.languages$name, msg = paste("Source language is not available in Google API Translation Please check. Must be any of \n", paste(available.languages$name, collapse = ", ")))
  assertthat::assert_that(target_lang %in% available.languages$name, msg = paste("Target language is not available in Google API Translation Please check. Must be any of \n", paste(available.languages$name, collapse = ", ")))

  # Get Target & Source Language
  target_lang_query <- available.languages[available.languages$name %in% target_lang, ]$language
  source_lang_query <- available.languages[available.languages$name %in% source_lang, ]$language

  # Identify how many characters will be requested
  char.to.request <- nchar(paste(dt[is.na(Translation)]$Text_Item, collapse = ""))
  message(paste(nrow(dt[is.na(Translation)]), "text items", paste0("(up to ", format(char.to.request, big.mark = ","), " characters)"), "will be queried for target language", paste0("'", target_lang, "'")))

  # QUERY FOR MISSING TRANSLATIONS
  dt[is.na(Translation), Status := "Machine"]
  dt[is.na(Translation), Translation :=
    suppressWarnings(googleLanguageR::gl_translate(
      Text_Item,
      source = source_lang_query,
      target = target_lang_query
    )$translatedText)]

  return(invisible(dt))
}





#' Query Google Translate API for text items in list of translations that are NA
#'
#'
#' @param trans.list List of translations as returned by [get_tms_data()] or [update_translations()]
#' @param languages Languages to be queried. By default uses names of `trans.list`
#' @param source_lang Source language, that is in which language questionnaire was designed. Default 'English'
#' @param target_languages Which languages of element names in 'trans.list' should be queried
#' @param auth Location of service account credential json file. Default in environment variable 'GL_AUTH'. For more details see  \code{vignette("setup", package = "googleLanguageR")}

#' @return List of translations
#' @export
#'
add_gl_translation <- function(trans.list = list(),
                               target_languages = NULL,
                               source_lang = "English",
                               auth = Sys.getenv("GL_AUTH")) {
  # TODO: ACTUALLY HERE ONLY THOSE WHICH ARE NOT IN PARTICULAR STATUS?

  # IF LANGUAGES NOT SPECIFIED, ASSUME ALL LANGUAGES IN LIST
  if (is.null(target_languages)) target_languages <- names(trans.list)
  assertthat::assert_that(all(target_languages %in% names(trans.list)), msg = paste(
    paste(target_languages[!target_languages %in% names(trans.list)], collapse = ","),
    "are not elements in trans.list. Please check."
  ))

  # Authenticate
  googleLanguageR::gl_auth(auth)



  # GO TRHOUGH ALL target_languages SPECIFIED
  purrr::walk(
    .x = target_languages,
    .f = ~ add_gl_translate_dt(trans.list[[.x]],
      source_lang = source_lang,
      target_lang = .x
    )
  )
}
