test_that("Test get_suso_tfiles with valid credentials", {

  # PUBLIC EXAMPLE Public example User questions and common patterns
  questionnaires <- "04b421596f0443e88cd99ec0538d89ff"
  sheets <- NULL

  result <- get_suso_tfiles(questionnaires,
    user = Sys.getenv("suso_designer_user"),
    password = Sys.getenv("suso_designer_pw"),
    sheets = sheets
  )
  expect_length(result, 1)
  expect_named(result, "Public example User questions and common patterns")
  # Check if the "Public example User questions and common patterns" element contains an element named "Translations"

  has_translations <- "Translations" %in% names(result[["Public example User questions and common patterns"]])
  expect_true(has_translations, info = "The element 'Public example User questions and common patterns' should contain at least one element named 'Translations'")

  # Check if the "Translations" element is of class data.table
  translations_element <- result[["Public example User questions and common patterns"]][["Translations"]]
  expect_s3_class(translations_element, "data.table")

  # Check if the "Translations" element has the specified column names
  expected_columns <- c("Entity Id", "Variable", "Type", "Index", "Original text", "Translation")
  actual_columns <- colnames(translations_element)
  expect_equal(actual_columns, expected_columns, info = "The 'Translations' element should have the specified column names")
})



#GET THE OBJECT FOR LATER USE
tmpl_list_testthat <- get_suso_tfiles("04b421596f0443e88cd99ec0538d89ff",
                          user = Sys.getenv("suso_designer_user"),
                          password = Sys.getenv("suso_designer_pw"),
                          sheets = NULL
)
