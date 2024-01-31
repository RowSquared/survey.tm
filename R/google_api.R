#' Add Google API  translation to data table. I.e. for an element of list of translations in tdb
#'
#' @param dt Data table of translations
#' @inheritParams  batchTranslate_GApi
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
  dt[is.na(Translation), Status := "machine"]
  dt[is.na(Translation), Translation :=
    suppressWarnings(googleLanguageR::gl_translate(
      Text_Item,
      source = source_lang_query,
      target = target_lang_query
    )$translatedText)]

  return(invisible(dt))
}





#' Batch Translate Text Items Using Google Translate API
#'
#' This function `batchTranslate_GApi` interfaces with the Google Translate API to process a list of data tables (`tdb`), each containing text items for translation. It queries the Google Translate API for translations of text items that are missing (NA) in the 'Translation' column. The function is adaptable to multiple target languages and works on each data table in the list based on specified languages.
#'
#' @param tdb List of translations as returned by [get_tdb_data()] or [update_tdbs()]. This list should contain data tables with text items ready for translation.
#' @param target_languages Languages to be queried for translation. By default, if not specified, the function uses the names of the data tables in `tdb` as target languages. Specify target languages as a vector of language names or codes supported by Google Translate.
#' @param source_lang Source language, indicating the original language of the text items. The default is 'English'. Accurate specification is crucial for effective translations.
#' @param auth Authentication details for Google Translate API. This can be specified as the location of the service account credential JSON file. The default is obtained from the environment variable 'GL_AUTH'. For more details on setting up authentication, see [Google Translate API Setup](https://cloud.google.com/translate/docs/setup).
#'
#' @return Returns a modified list of data tables (`tdb`) where the text items have been translated to the specified target languages. Each data table in the list will have updated translations for the previously missing items.
#' @export
#'
#' @examples
#' \dontrun{

#' # Example usage (assuming 'tdb' is a predefined list of data tables):
#' translated_tdb <- batchTranslate_GApi(tdb, target_languages = c("German", "French"), auth = my_auth_file)
#' }
#'
#' @seealso
#' `gl_translate` in the `googleLanguageR` package for details on the underlying translation function.
#' `get_tdb_data()` and `update_tdbs()` for functions generating the input list of data tables.
#'
#'
batchTranslate_GApi <- function(tdb = list(),
                               target_languages = NULL,
                               source_lang = "English",
                               auth = Sys.getenv("GL_AUTH")) {

  # IF LANGUAGES NOT SPECIFIED, ASSUME ALL LANGUAGES IN LIST
  if (is.null(target_languages)) target_languages <- names(tdb)
  assertthat::assert_that(all(target_languages %in% names(tdb)), msg = paste(
    paste(target_languages[!target_languages %in% names(tdb)], collapse = ","),
    "are not elements in tdb. Please check."
  ))

  # Authenticate
  googleLanguageR::gl_auth(auth)



  # GO TRHOUGH ALL target_languages SPECIFIED
  purrr::walk(
    .x = target_languages,
    .f = ~ add_gl_translate_dt(tdb[[.x]],
      source_lang = source_lang,
      target_lang = .x
    )
  )
}
