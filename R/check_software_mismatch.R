#' Check individual sheet (data.table) for html-tag issues
#'
#' @param dt data table of a translation
#' @param lang name of sheet/language. for message
#'
#' @return data table with text items that contain issues
#' @noRd
get_htmltag_issue_dt <- function(dt,
                                 lang=NULL) {

  # Extract tags function
  html_tag_pattern <- "<(?:font color=\"[^\"]+\"|/?[a-zA-Z]+)>"
  extract_tags <- function(text) {
    unlist(regmatches(text, gregexpr(html_tag_pattern, text)))
  }
  # Count html tags
  count_tags <- function(text) {
    tags <- extract_tags(text)
    table(tags)
  }
  #Run it
  dt[, Text_Item_Tags := lapply(Text_Item, count_tags)]
  dt[, Translation_Tags := lapply(Translation, count_tags)]


  # Comparison function
  compare_tags <- function(tags1, tags2) {
    all(sort(names(tags1)) == sort(names(tags2)) &
          all(tags1[sort(names(tags1))] == tags2[sort(names(tags2))]))
  }

  # Apply the comparison function to each row
  dt[, Tags_Equivalent := mapply(compare_tags, Text_Item_Tags, Translation_Tags)]

  #Keep rows with issues and apply a comment
  # Convert the two list column to a character column
  paste.list <- function(tags) paste(paste(names(tags), tags, sep="="), collapse="; ")
  dt[Tags_Equivalent==FALSE,
       comment.issue :=

         paste0("Difference in count of html-tag:\n",
           #Original/Text_Item
           "Text_Item:",sapply(Text_Item_Tags, \(x) paste.list(x)),
                "\n","Translation:",
                sapply(Translation_Tags, \(x) paste.list(x)))

    ]
  #Keep cols & rows of interest (Just value.unique and comment.issue). Will be processed in parent function.
   dt <- dt[Tags_Equivalent==FALSE,.(value.unique,
                                  comment.issue)]


  #Print results
  message(paste(nrow(dt),"text items with html-tag issues for language",paste0("'",lang,"'")))

  return(dt)
}




#' Check individual sheet (data.table) for text substitution issues
#'
#' @param dt data table of a translation
#' @param pattern pattern that identifies Text substitution in Original/Translation (e.g. text substitution '%rostertitle%')
#' @param lang name of sheet/language. for message
#'
#' @return data table with text items that contain issues
#' @noRd
get_txt_sub_issue_dt <- function(dt,
                           pattern="%[a-zA-Z0-9_]+%",
                           lang=NULL) {

  #Extract the text items per pattern into list column
  dt <- dt[grepl(pattern,Text_Item)|grepl(pattern,Translation),
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
  message(paste(nrow(dt),"text items with text substitution issues for language",paste0("'",lang,"'")))

  return(dt)
}




#' Identifies software-related mismatches between 'Text_Item' and 'Translation'
#'
#' Updates column 'Status' and leaves a 'Comment/Note' that explains which issue is identified.
#'
#' @param tdb List. Translations created by `get_tdb_data()` or `update_tdb()`
#' @param pattern Character. Regular expression that identifies text item in Original/Translation (e.g. text substitution '%rostertitle%')
#' @param languages Character Vector. For which sheets/languages shall software-related mismatches be checked?
#'
#' @return List of translations as supplied by user, with software mismatches flagged
#' @export
#'
syntax_check <- function(
    tdb = list(),
    pattern="%[a-zA-Z0-9_]+%"
) {
  # tdb <- new_tdb
  #Copy list to avoid replacement in place
  tdb <- copy(tdb)

  #Identify Text Substitution issues for each translation - But the one in Status "outdated"
  list.txtsub.issues <- purrr::map(
    .x = names(tdb),
    .f = ~ get_txt_sub_issue_dt(new_tdb[[.x]][!Status %chin% c("outdated")],
                               lang=.x)
  )
  list.txtsub.issues <- setNames(list.txtsub.issues, names(tdb))

  #Identify html-tag issues
  list.html.issues <- purrr::map(
    .x = names(tdb),
    .f = ~ get_htmltag_issue_dt(new_tdb[[.x]][!Status %chin% c("outdated")],
                                lang=.x)
  )
  list.html.issues <- setNames(list.html.issues, names(tdb))

  #Bind both list of issues into one list
  full.list.issues <- lapply(names(list.txtsub.issues), function(name) {
    rbind(list.txtsub.issues[[name]], list.html.issues[[name]])
  })
  #Collapse to value.unique as an item could have both issues
  collapse.operation <- function(dt) dt[,.(comment.issue=paste(comment.issue, collapse = "\n\n")),by=.(value.unique)]
  full.list.issues <- lapply(full.list.issues, collapse.operation)
  names(full.list.issues) <- names(list.txtsub.issues)

  #Update each Translation sheet
  updated.list <- lapply(names(tdb), \(x) {

    # Get row identifier
    tdb[[x]][, seq.id := 1:.N]

    #Update Status
    tdb[[x]][value.unique %in%  full.list.issues[[x]]$value.unique,Status:="to be checked"]

    #Get in issues
    tdb[[x]] <- merge(tdb[[x]],full.list.issues[[x]],by="value.unique",all.x=T)

    ###Update Comment###
    #Replace if Comment/Note empty
    tdb[[x]][is.na(`Comment/Note`) & !is.na(comment.issue),
          `Comment/Note`:=comment.issue]

    #Clear Comment/note if no issue is present any longer (and any software comment in there)
    tdb[[x]][grepl("not found in|Count of text substitution",`Comment/Note`)
          & is.na(comment.issue), `Comment/Note`:=NA]

    #Back in order as received
    setorder(tdb[[x]],seq.id)
    tdb[[x]][,c("comment.issue","seq.id"):=NULL]

  })
  updated.list <- setNames(updated.list, names(tdb))

  #Return the list
  return(updated.list)

}





