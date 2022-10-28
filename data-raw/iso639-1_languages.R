## code to prepare data table of all ISO 639-1languages dataset goes here

library(data.table)
library(rvest)
library(stringr)

#Retrieve ISO 639-1 from Wikipedia
languages <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes")
languages <- as.data.table(languages %>%
                html_nodes("#Table") %>%
                html_table(fill=TRUE))

#Add languagnes "also known as" language.
languages[grepl("also known as",Notes),ISO.language.name:=
            paste(
              ISO.language.name,stringr::
            str_replace_all(stringr::str_remove_all(Notes,"also known as |macrolanguage, |ancient, ")," or ",", ")
            ,sep=", ")]

usethis::use_data(languages, overwrite = TRUE,internal = TRUE)
