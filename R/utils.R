#' CLEANUP TEXT ITEM - REMOVE ONLY PROPER WHITESPACE TO NOT REMOVE TABS AND NEWLINES
#'
#' @importFrom stringr str_to_lower str_remove_all
#'
cleanup.text.item <- function(dt,regex=" ") {
  dt[,value.unique:=stringr::str_to_lower(stringr::str_remove_all(value,regex))]
}



#' Check if named vector of character type
#'
#' @importFrom stringr str_to_lower str_remove_all
#'

is.char.named.vector <- function(vec) {
  is.vector(vec)  & is.character(vec) &  !is.null(names(vec)) &
    !any(is.na(names(vec)))
}
