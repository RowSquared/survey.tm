
#' Check individual sheet (data.table) for software mismatches
#'
#' @param dt data table of a translation
#' @param pattern pattern that identifies text item in Original/Translation (e.g. text substitution '%rostertitle%')
#' @param lang name of sheet/language. for message
#'
#' @return data table with text items that contain issues
#' @noRd
get_sw_issues_dt <- function(dt,
                           pattern="%[a-zA-Z0-9_]+%",
                           lang=NULL) {

  #Extract the text items per pattern into list column
  dt <- dt[grepl(pattern,Text_Item),
           .(orig=stringr::str_extract_all(Text_Item,pattern),
             trans=stringr::str_extract_all(Translation,pattern)
           ),by=.(value.unique)]

  #Identify mismatches
  dt <- dt[,.(
    #Item is in Original but not Translation
    not.trans=list(setdiff(unlist(orig),unlist(trans))),
    #Item is in Translation but not Original
    not.orig=list(setdiff(unlist(trans),unlist(orig))),
    #Get count of items. Is one item used more often than the other?
    diff.length=length(unlist(trans))!=length(unlist(orig))),by=.(value.unique)]


  dt[,`:=` (any.not.trans=length(unlist(not.trans))>0,
            any.not.orig=length(unlist(not.orig))>0),
     by=.(value.unique)]


  #Bind the issues
  dt <- rbindlist(list(
    #Not found in Trans
    dt[any.not.trans==T,
       .(comment.issue=paste(paste(unlist(not.trans),
                                   collapse=", "),"not found in Translation")),
       by=.(value.unique)],
    #Not found in Orig
    dt[any.not.orig==T,
       .(comment.issue=paste(paste(unlist(not.orig),
                                   collapse=", "),"not found in Original")),
       by=.(value.unique)],
    #Item Count not aligned
    dt[any.not.trans==F & any.not.orig==F & diff.length==T,
       .(value.unique,
         comment.issue="Count of text substitution between Original and Translation does not match."
       )]

  ))

  #Get Issues by unique Text Item
  dt <- dt[,.(comment.issue=paste(comment.issue, collapse = "\n")),by=.(value.unique)]

  #Print results
  message(paste(nrow(dt),"text items with software-related mismatches for language",paste0("'",lang,"'")))

  return(dt)
}




#' Identifies software-related mismatches between 'Text_Item' and 'Translation'
#'
#' Updates column 'Status' and leaves a 'Comment/Note' that explains which issue is identified.
#'
#' @param trans.list List. Translations created by `get_tdb_data()` or `update_tdb()`
#' @param pattern Character. Regular expression that identifies text item in Original/Translation (e.g. text substitution '%rostertitle%')
#' @param languages Character Vector. For which sheets/languages shall software-related mismatches be checked?
#'
#' @return List of translations as supplied by user, with software mismatches flagged
#' @export
#'
identify_sw_issues <- function(
    trans.list = list(),
    pattern="%[a-zA-Z0-9_]+%"
) {

  #Copy list to avoid replacement in place
  trans.list <- copy(trans.list)

  #Identify issues for each translation
  list.issues <- purrr::map(
    .x = names(trans.list),
    .f = ~ get_sw_issues_dt(new_tdb[[.x]],
                               lang=.x)
  )
  list.issues <- setNames(list.issues, names(trans.list))

  #Update each Translation sheet
  updated.list <- lapply(names(trans.list), \(x) {

    # Get row identifier
    trans.list[[x]][, seq.id := 1:.N]

    #Update Status
    trans.list[[x]][value.unique %in%  list.issues[[x]]$value.unique,Status:="to update"]

    #Get in issues
    trans.list[[x]] <- merge(trans.list[[x]],list.issues[[x]],by="value.unique",all.x=T)

    ###Update Comment###
    #Replace if Comment/Note empty
    trans.list[[x]][is.na(`Comment/Note`) & !is.na(comment.issue),
          `Comment/Note`:=comment.issue]

    #Clear Comment/note if no issue is present any longer (and any software comment in there)
    trans.list[[x]][grepl("not found in|Count of text substitution",`Comment/Note`)
          & is.na(comment.issue), `Comment/Note`:=NA]

    #Back in order as received
    setorder(trans.list[[x]],seq.id)
    trans.list[[x]][,c("comment.issue","seq.id"):=NULL]

  })
  updated.list <- setNames(updated.list, names(trans.list))

  #Return the list
  return(updated.list)

}





