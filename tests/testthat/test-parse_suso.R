#GET THE OBJECT FOR LATER USE
tmpl_list_testthat <- get_suso_tfiles("04b421596f0443e88cd99ec0538d89ff",
                                      user = Sys.getenv("suso_designer_user"),
                                      password = Sys.getenv("suso_designer_pw"),
                                      sheets = NULL
)


# Test if the function returns a data.table object
test_that("parse_suso_titems returns a data.table object", {
  result <- parse_suso_titems(tmpl_list_testthat)
  expect_true(is.data.table(result))
})

#TODO: WORK WITH A STATIC QUESTIONNAIRE
# Test if the function returns only unique text items when 'collapse = TRUE'
test_that("parse_suso_titems returns unique text items when collapse is TRUE", {
  result <- parse_suso_titems(tmpl_list_testthat, collapse = TRUE)
  expect_true(all(duplicated(result$text) == FALSE))
})

# Test if the function returns all text items when 'collapse = FALSE'
test_that("parse_suso_titems returns all text items when collapse is FALSE", {
  result.1 <- parse_suso_titems(tmpl_list_testthat, collapse = TRUE)
  result.2 <- parse_suso_titems(tmpl_list_testthat, collapse = FALSE)
  expect_true(nrow(result.1)<nrow(result.2))
})


# Test if the function returns only the specified 'types'
test_that("parse_suso_titems returns only specified types", {
  types_to_test <-  c(
    "Title", "Instruction", "OptionTitle", "ValidationMessage",
    "SpecialValue", "FixedRosterTitle"
  )
  result <- parse_suso_titems(tmpl_list_testthat, types = types_to_test)
  expect_true(all(result$type %in% types_to_test))

})
