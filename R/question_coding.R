

#' Remove the question coding of a text item
#'
#' Identifies and removes supplied string pattern in dt to get rid of Question Coding.
#' Can be used to reduce workload for Translators as one does not need to include Question Coding in Translation (which reduces likelihood of typos/mistakes) and
#' can further collapse data table by removing potential duplicate text items for which only question coding differs
#'
#' @param dt  Data table of questionnaire text items returned by either [parse_odk_titems()] or [parse_suso_titems()]
#' @param pattern Regular expression that matches question coding
#' @param collapse boolean to indicate if after removing question coding, the set of translation items should be scanned for/collapsed to unique items
#'
#' @return dt
#' @export
#'
remove_coding <- function(dt,
                          pattern=stop("'pattern' must be specified"),
                          collapse=TRUE) {
  #TODO: Check Input

  #Get copy of dt, as in place changes are made. If no object assigned with funciton might cause issues
  dt <- copy(dt)
  #Store nrow of initial dt so we can display result in end
  init.row <- nrow(dt)
  #Display number of text items
  message(paste(nrow(dt[grepl(pattern,value)]),"text items identified for which Coding will be removed"))
  #Remove
  dt[grepl(pattern,value),
     `:=` (
       #Value unique: Remove pattern but based on Original Text Item as it
       #could contain whitespace
       value.unique=stringr::str_remove(value.unique,
                                        stringr::str_to_lower(stringr::str_remove_all(
                                          stringr::str_extract(value,pattern),
                                          " "))),
       #Value, simply remove user pattern
       value=stringr::str_remove(value,pattern))
  ]
  #Collapse now with new
  if (collapse) {
    dt <- collapse_titems(dt)
    message(paste(init.row-nrow(dt), "rows removed.",
                  sprintf("%.1f%%", (1-nrow(dt)/init.row)*100),"of init dataset."))
  }

  return(dt)

}
