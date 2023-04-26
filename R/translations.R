#' Update translation for one element in list
#'
#' @inheritParams  update_translation
#' @noRd
update_translation_lelement <- function(curr.trans,
                                        new.items) {

  # First Scenarion: Text Items exist in current Translation sheet and is found again in Master Questionnaire
  dt1 <- curr.trans[as.character(value.unique) %chin% new.items$value.unique]

  # Second Scenario: New items in Master Questionnaire not found yet in Translation Sheet
  dt2 <- new.items[
    !value.unique %chin% as.character(curr.trans$value.unique),
    .(value.unique,
      `Questionnaire(s)` = questionnaire,
      Type = type,
      Text_Item = value
    )
  ]

  # Bind to one
  dt <- rbindlist(list(
    dt1, dt2
  ), fill = TRUE)

  # Set Status to "to translate" if NA
  dt[is.na(Translation), Status := "to translate"]

  #Get in current sequential order, using the Master Questionnaire as reference
  dt <- merge(dt,new.items[,.(value.unique,seq.id)],by="value.unique",all.x=T)
  setorder(dt,seq.id)
  dt[,"seq.id":=NULL]

  return(dt)
}


#' Compares current questionnaire file against list of existing translations
#'
#' Removes any text item that no longer is part of the questionnaire(s).
#' Adds any text item that was not part of translation before
#'
#' @param curr.trans List of translations as returned by [get_tms_data()]
#' @param new.items Data table of questionnaire text items returned by either [parse_odk_titems()] or [parse_suso_titems()]
#'
#' @return List of updated translations
#'
#' @export
#'
update_translation <- function(curr.trans = list(),
                               new.items = data.table()) {
  assertthat::assert_that(is.list(curr.trans))
  assertthat::assert_that(is.data.table(new.items))

  # Identify languages in current list of translations
  languages <- names(curr.trans)

  # Go through all sheets of current translation and compare against master
  updated.trans.sheets <- purrr::map(
    .x = languages,
    .f = ~ update_translation_lelement(
      curr.trans = curr.trans[[.x]],
      new.items = new.items
    )
  )
  updated.trans.sheets <- setNames(updated.trans.sheets, c(languages))

  return(updated.trans.sheets)
}

