#' Add DeepL translation to data table. I.e. for an element of list of
#'
#' @param dt Data table of translations
#' @inheritParams  add_deepl_translation
#'
#'
add_deepl_translation_dt <- function(dt,
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
  dt[is.na(Translation), Status := "Machine"]
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





#' Query DeepL translation for text items in list of translations that are NA
#'
#' @param trans.list List of translations as returned by [get_translations()] or [update_translations()]
#' @param target_languages  Languages to be queried. By default uses names of `trans.list`
#' @param source_lang Source language, that is in which language questionnaire was designed. Default 'English'
#' @param API_key Authentication Key for DeepL API
#'
#' @importFrom  deeplr available_languages2 translate2
#'
#' @return List of translations
#' @export
#'
add_deepl_translation <- function(trans.list = list(),
                                  target_languages = NULL,
                                  source_lang = "English",
                                  API_key = NULL) {
  #TODO: HAVE USER DECIDE IF FREE OR PAID API!
  # IF LANGUAGES NOT SPECIFIED, ASSUME ALL LANGUAGES IN LIST
  if (is.null(target_languages)) target_languages <- names(trans.list)
  assertthat::assert_that(all(target_languages %in% names(trans.list)), msg = paste(
    paste(target_languages[!target_languages %in% names(trans.list)], collapse = ","),
    "are not elements in trans.list. Please check."
  ))

  # GO TRHOUGH ALL target_languages SPECIFIED
  purrr::walk(
    .x = target_languages,
    .f = ~ add_deepl_translation_dt(trans.list[[.x]],
      source_lang = source_lang,
      target_lang = .x,
      API_key = API_key
    )
  )
}
