#' Add DeepL translation to data table. I.e. for an element of list of
#'
#' @param dt Data table of translations
#' @inheritParams  batchTranslate_Deepl2
#'
#' @noRd
batchTranslate_Deepl2_dt <- function(dt,
                                     source_lang = "English",
                                     target_lang = NULL,
                                     API_key = NULL) {
  # Check input

  if (!requireNamespace("deeplr", quietly = TRUE)) {
    stop(
      "Package \"deeplr\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # TODO: ASSERT THAT ALL COLUMNS THAT ARE EXPECTED ARE IN dt
  # TODO: CHECK IF ONE QUERY RETURNING MULTIPLE LANGUAGES SAVES CHARACTERS?
  # CHECK INPUT
  # Any translation actually missing?

  if (nrow(dt[is.na(Translation)]) == 0) {
    message(paste("dt has no row with missing 'Translation'. DeepL will not be queried for", target_lang))
    return(invisible(dt))
  }

  # Languages correctly specified?
  available.languages <- deeplr::available_languages2(API_key)
  assertthat::assert_that(source_lang %in% available.languages$name, msg = paste("Source language is not available in DeepL. Please check. Must be any of \n", paste(available.languages$name, collapse = ", ")))
  assertthat::assert_that(target_lang %in% available.languages$name, msg = paste("Target language is not available in DeepL. Please check. Must be any of \n", paste(available.languages$name, collapse = ", ")))

  # PRINT CURRENT STATS
  stats <- deeplr::usage2(API_key)
  stats$left <- stats$character_limit - stats$character_count
  message(paste(
    format(stats$character_count, big.mark = ","), "out of", format(stats$character_limit, big.mark = ","),
    sprintf("(%.1f%%)", (stats$character_count / stats$character_limit) * 100), "characters used with this API credential"
  ))

  # Assert that new querie is within limit
  char.to.request <- nchar(paste(dt[is.na(Translation)]$Text_Item, collapse = ""))
  message(paste(nrow(dt[is.na(Translation)]), "text items", paste0("(up to ", format(char.to.request, big.mark = ","), " characters)"), "will be queried for target language", target_lang))
  assertthat::assert_that(char.to.request <= stats$left,
    msg = paste(
      "Characters to be requested: ",
      format(char.to.request, big.mark = ","), "\nCharacters left in API account:    ", format(stats$left, big.mark = ",")
    )
  )


  # QUERY FOR MISSING TRANSLATIONS
  dt[is.na(Translation), Status := "machine"]
  dt[is.na(Translation), Translation :=
    deeplr::translate2(
      text = c(Text_Item),
      source_lang = available.languages[available.languages$name %in% source_lang, ]$language,
      target_lang = available.languages[available.languages$name %in% target_lang, ]$language,
      auth_key = (API_key)
    )]

  # #PRINT STATS AFTER QUERY
  # stats <- deeplr::usage2(API_key)
  # stats$left <- stats$character_limit-stats$character_count
  # message(paste(format(stats$character_count,big.mark = ","),"out of", format(stats$character_limit,big.mark = ","),
  #               sprintf("(%.1f%%)",(stats$character_count/stats$character_limit)*100),"characters left with this API credential"))

  return(invisible(dt))
}





#' Batch Translate Text Items Using DeepL
#'
#' This function serves as a wrapper for specific functions from the `deeplr` package,
#' utilizing the "2" functions (like `translate2`) which are compatible with the free account of DeepL.
#'
#' It processes a list of data tables, each representing a set of translations, and queries the DeepL API for translations of text items that are missing (NA) in column 'Translation'.
#' The function is designed to handle multiple languages and can process each data table in the list according to the specified target languages.
#'
#' @param tdb List of translations as returned by [get_tdb_data()] or [update_tdb()]. This list should contain data tables with text items to be translated.
#' @param target_languages Languages to be queried for translation. By default, the function uses the names of the data tables in `tdb` as the target languages. If specified, it should be a vector of language names or codes that are supported by DeepL.
#' @param source_lang Source language, indicating the language in which the questionnaire or text items were originally designed. The default is 'English'. It's important to specify this accurately to ensure correct translations.
#' @param API_key Authentication Key for the DeepL API. Ensure that this key is valid for the free account of DeepL if using functions specific to that account type. For information on obtaining an API key, visit [DeepL API](https://www.deepl.com/en/pro-api?utm_campaign=Google_EMEA_EN_Search_B_Brand_Conversion&utm_term=deepl%20api%20key&utm_source=adwords&utm_medium=ppc&gad_source=1).
#'
#' @return Returns a modified list of data tables (`tdb`) where the text items have been translated as per the specified target languages. Each data table in the list will have updated translations where previously missing.
#' @export
#' @examples
#' \dontrun{
#' # Example usage (assuming 'tdb' is a predefined list of data tables and 'API_key' is set):
#' translated_tdb <- batchTranslate_Deepl2(tdb, target_languages = c("German", "French"), API_key = my_api_key)
#' }
#' @seealso
#' `translate2` in the `deeplr` package for details on the underlying translation function.
#' `get_tdb_data()` and `update_tdb()` for functions generating the input list of data tables.
#'
batchTranslate_Deepl2 <- function(tdb = list(),
                                  target_languages = NULL,
                                  source_lang = "English",
                                  API_key = NULL) {

  # IF LANGUAGES NOT SPECIFIED, ASSUME ALL LANGUAGES IN LIST
  if (is.null(target_languages)) target_languages <- names(tdb)
  assertthat::assert_that(all(target_languages %in% names(tdb)), msg = paste(
    paste(target_languages[!target_languages %in% names(tdb)], collapse = ","),
    "are not elements in tdb. Please check."
  ))

  # GO TRHOUGH ALL target_languages SPECIFIED
  purrr::walk(
    .x = target_languages,
    .f = ~ batchTranslate_Deepl2_dt(tdb[[.x]],
      source_lang = source_lang,
      target_lang = .x,
      API_key = API_key
    )
  )
}
