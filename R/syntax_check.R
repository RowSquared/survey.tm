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
  # Run it
  dt[, Text_Item_Tags := lapply(Text_Item, count_tags)]
  dt[, Translation_Tags := lapply(Translation, count_tags)]
  # Comparison function
  compare_tags <- function(tags1, tags2) {
    # If both lists are empty, they are equivalent
    if (length(tags1) == 0 && length(tags2) == 0) {
      return(TRUE)
    }

    # If one list is empty but the other is not, they are not equivalent
    if (length(tags1) == 0 || length(tags2) == 0) {
      return(FALSE)
    }

    # Convert to named vectors (if they are not already)
    tags1 <- as.vector(tags1)
    names(tags1) <- names(tags1)
    tags2 <- as.vector(tags2)
    names(tags2) <- names(tags2)

    # Sort by names
    # tags1 <- tags1[order(names(tags1))]
    # tags2 <- tags2[order(names(tags2))]

    # Check if they have the same names and the same counts
    identical(tags1, tags2)
  }
  # Apply the comparison function to each row
  dt[, Tags_Equivalent := mapply(compare_tags, tags1=Text_Item_Tags,
                                                            tags2=Translation_Tags)]


  # Keep rows with issues and apply a comment
  # Convert the two list column to a character column
  paste.list <- function(tags) paste(paste(names(tags), tags, sep = "="), collapse = "; ")
  dt[
    Tags_Equivalent == FALSE,
    comment.issue :=
      paste0(
        "Difference in count of html-tag:\n",
        # Original/Text_Item
        "Text_Item:", sapply(Text_Item_Tags, function(x) paste.list(x)),
        "\n", "Translation:",
        sapply(Translation_Tags, function(x) paste.list(x))
      )
  ]
  # Keep cols & rows of interest (Just value.unique and comment.issue). Will be processed in parent function.
  dt <- dt[Tags_Equivalent == FALSE, .(
    value.unique,
    comment.issue
  )]


  # Print results
  message(paste(nrow(dt), "text items with html-tag issues for language", paste0("'", lang, "'")))

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
  # Copy list to avoid replacement in place
  tdb.fun <- copy(tdb)

  #Identify Text Substitution issues for each translation - But the one in Status "outdated"
  list.txtsub.issues <- purrr::map(
    .x = names(tdb),
    .f = ~ get_txt_sub_issue_dt(tdb.fun[[.x]][!Status %chin% c("outdated") & !is.na(Translation)],
                               lang=.x)
  )
  list.txtsub.issues <- setNames(list.txtsub.issues, names(tdb.fun))

  #Identify html-tag issues
  list.html.issues <- purrr::map(
    .x = names(tdb.fun),
    .f = ~ get_htmltag_issue_dt(tdb.fun[[.x]][!Status %chin% c("outdated") & !is.na(Translation)],
                                lang=.x)
  )
  list.html.issues <- setNames(list.html.issues, names(tdb.fun))

  #Bind both list of issues into one list
  full.list.issues <- lapply(names(list.txtsub.issues), function(name) {
    rbind(list.txtsub.issues[[name]], list.html.issues[[name]])
  })
  #Collapse to value.unique as an item could have both issues
  collapse.operation <- function(dt) dt[,.(comment.issue=paste(comment.issue, collapse = "\n\n")),by=.(value.unique)]
  full.list.issues <- lapply(full.list.issues, collapse.operation)
  names(full.list.issues) <- names(list.txtsub.issues)

  #Update each Translation sheet
  updated.list <- lapply(names(tdb.fun), function(x) {

    # Get row identifier
    tdb.fun[[x]][, seq.id := 1:.N]

    #Update Status
    tdb.fun[[x]][value.unique %in%  full.list.issues[[x]]$value.unique,Status:="to be checked"]

    #Get in issues
    tdb.fun[[x]] <- merge(tdb.fun[[x]],full.list.issues[[x]],by="value.unique",all.x=T)

    ###Update Comment###
    #Replace if Comment/Note empty
    tdb.fun[[x]][is.na(`Comment/Note`) & !is.na(comment.issue),
          `Comment/Note`:=comment.issue]

    #If not empty & no SW issue just yet, add to existing comment
    tdb.fun[[x]][!is.na(`Comment/Note`) & !is.na(comment.issue) & !grepl("count of html|not found in|Count of text substitution",`Comment/Note`),
             `Comment/Note`:=paste0(`Comment/Note`,"\n\n",comment.issue)]

    #Clear Comment/note if no issue is present any longer (and any software comment in there)
    tdb.fun[[x]][grepl("count of html|not found in|Count of text substitution",`Comment/Note`)
          & is.na(comment.issue), `Comment/Note`:=NA]


    #Back in order as received
    setorder(tdb.fun[[x]],seq.id)
    tdb.fun[[x]][,c("comment.issue","seq.id"):=NULL]

  })
  updated.list <- setNames(updated.list, names(tdb.fun))

  #Return
  return(updated.list)

}





