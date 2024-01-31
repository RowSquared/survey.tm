#' Internal String Similarity Calculation
#'
#' @description This is an internal function to compute the similarity between two strings.
#' @param target_str The target string for which a reference will be looked up.
#' @param reference_vector A vector in which existing translations are stored.
#' @param reference_text_items A vector containing the 'Human readable' original text items.
#' @param reference_translations A vector containing the corresponding translations.
#' @param method The method to use for computing string similarity, with "jw" (Jaro-Winkler) as default.
#' @return A list containing the most similar text item, its translation, and the similarity distance.
#' @keywords internal
#TODO: Very clumpsy with providing three vectors. Do a simple dt with the three cols?
string_similarity <- function(target_str,
                              reference_vector,
                              reference_text_items,
                              reference_translations,
                              method="jw"
) {

  # Compute distances based on the chosen method
  if (method == "lv") {
    distances <- stringdist::stringdistmatrix(target_str, reference_vector, method = "lv")
  } else if (method == "jw") {
    distances <- stringdist::stringdistmatrix(target_str, reference_vector, method = "jw")
  } else {
    stop("Unknown method!")
  }

  # Get the index of the string with the smallest distance
  closest_index <- which.min(distances)

  #Return result + Accompanying Text_Item & Translation
  list(ref_text_item = reference_text_items[closest_index],
       ref_translation=reference_translations[closest_index],
       distance = distances[closest_index])
}






#' Lookup Translations for Untranslated Text Items
#'
#' @description This function identifies text items without translations and looks up the closest match in the reference translations.
#' @param dt A data.table object containing text items and translations for a specific language.
#' @param min.dist The minimum distance threshold. Text items with similarity distance greater than this threshold will not get a matched reference. The closer the number is to 0, the more similar the two strings. If the number is 1, the strings are completely different.
#' @return A modified data.table with additional column 'Similarity' for matched reference translations and similarity details.
#' @examples
#'   \dontrun{
#' # Assuming `new_tdb` is a list of data.tables for different languages as returned by `update_tdb()`
#' lookup_translation(new_tdb$Portuguese)
#'}
lookup_translation <- function(dt,  min.dist = 0.2) {
  #TODO: A lot! Document that it simply takes an updated Translation Database Object and uses those which have a translation as reference "data" and those not will be looked up in those reference stuff.
  # dt <- new_tdb$Portuguese
  # min.dist=0.2


  ## 1) IDENTIFY IF FUNCTION NEEDS TO RUN

  # Check if actually any work is needed
  if (nrow(dt[is.na(Translation)]) == 0) {
    message("All text items already have translations. No lookups needed.")
    return()
  }

  if (nrow(dt[!is.na(Translation)]) == 0) {
    message("There are no reference translations available. Cannot perform lookups.")
    return()
  }

  ## 2) PROCESS THE STRING
  #Remve all punctuation (but %) & whitespace. Remove als "<" & ">" from html tags.
  #Do not use unique.var for now
  #TODO: Investigate further which implications it has / what could be improved
  punctuations <- "[.,!?;:\\-_'\"\\[\\]{}<>|/\\\\&*+=^$#@~]"

  dt[,cleaned_textitem:=stringr::str_to_lower(
    stringr::str_remove_all(Text_Item,paste(punctuations,"\\s+",sep="|")))]


  ## 3) Compute the closest strings and distances
  results <- lapply(dt[is.na(Translation)]$cleaned_textitem,
                    string_similarity, reference_vector = dt[!is.na(Translation)]$cleaned_textitem,
                    reference_text_items=dt[!is.na(Translation)]$Text_Item,
                    reference_translations=dt[!is.na(Translation)]$Translation)


  # Add results to the data.table - CLumpsy 4 now
  dt[is.na(Translation), ref_text_item := sapply(results, `[[`, "ref_text_item")]
  dt[is.na(Translation), ref_translation := sapply(results, `[[`, "ref_translation")]
  dt[is.na(Translation), distance := as.numeric(sapply(results, `[[`, "distance"))]


  #Remove the info if above certrain threshold
  dt[distance>min.dist,c("ref_text_item","ref_translation","distance"):=NA]


  #Collapse all into one col for now
  dt[distance<=min.dist,Similarity:=paste(paste("Distance:",round(distance,digits=4)),
                                          ref_text_item, ref_translation,sep="\n\n")]


  #Cleanup
  dt[,c("cleaned_textitem","ref_text_item","ref_translation","distance"):=NULL]


  return(invisible(dt))

}



