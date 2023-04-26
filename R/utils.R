#' CLEANUP TEXT ITEM - REMOVE ONLY PROPER WHITESPACE TO NOT REMOVE TABS AND NEWLINES
#'
#' @importFrom stringr str_to_lower str_remove_all
#' @noRd
create.unique.var <- function(dt, regex = " ",col="value") {
  dt[, value.unique := stringr::str_to_lower(stringr::str_remove_all(get(col), regex))]
}



#' Check if named vector of character type
#'
#' @importFrom stringr str_to_lower str_remove_all
#' @noRd
is.char.named.vector <- function(vec) {
  check <- is.vector(vec) & is.character(vec) & !is.null(names(vec)) &
    !any(is.na(names(vec))) & !any(names(vec) %in% "")

  return(check)
}


#' COLLAPSE dt OF TRANSLATION ITEMS INTO UNIQUE SET OF TRANSLATION ITEMS
#'
#' @param dt
#'
#' @return data table of unique items
#' @noRd
collapse_titems <- function(dt) {

  # COLLAPSE AND REMOVE IDENTIFIER
  # TODO: ASSERT THAT DT SUPPLIED FOLLOWS STANDARD
  # TODO: CODE CAN BE SIMPLIFIED/BEAUTIFIED


  #ADD SEQUENTIAL IDENTIFIER IF NOT EXISTENT. MAINLY IF USED FOR remove_coding
  if (!"seq.id" %in% names(dt)) dt[, seq.id := 1:.N]


  # KEEP UNIQUE:
  # ALWAYS ACCOUNT FOR SEQUENTIAL ID
  # FIRST BY QUESTIONNAIRE; VARIABLE AND VALUE TO AVOID HAVING MULTIPLE "label, label, label"
  dt <- dt[
    , .(
      seq.id = seq.id[c(1)],
      value = value[c(1)]
    ),
    by = .(questionnaire, type, value.unique)
  ]

  # NOW BY TYPE AND VALUE
  dt <- dt[
    , .(
      seq.id = seq.id[c(1)],
      value = value[c(1)],
      questionnaire = paste(questionnaire, collapse = "\n")
    ),
    by = .(type, value.unique)
  ]

  # NOW BY UNIQUE VALUE, WITH TYPE OF VARIABLE COLLAPSED
  setorder(dt, value.unique, seq.id)
  dt <- dt[, .(
    seq.id = seq.id[c(1)],
    questionnaire = questionnaire[c(1)],
    value = value[c(1)],
    type = paste(type, collapse = "\n")
  ), by = .(value.unique)]

  #SET ORDER OF APPEARANCE AND REMOVE IDENTIFIER
  setorder(dt, seq.id)
  # dt[, seq.id := NULL]

  return(dt)

}

