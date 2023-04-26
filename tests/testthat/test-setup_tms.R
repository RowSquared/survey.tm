test_that("setup_tms creates a new sheet with the specified languages", {
  test_ssheet_name <- "MyTestTranslationSheet"
  test_lang_names <- c("English", "Occitan")

  # Create a new sheet
  new_sheet <- setup_tms(ssheet_name = test_ssheet_name, lang_names = test_lang_names)

  # Check if the created sheet has the correct name
  expect_equal(new_sheet$name, test_ssheet_name)

  # Check if the correct number of sheets (languages) are created
  expect_equal(nrow(new_sheet$sheets), length(test_lang_names))

  # Check if the sheet names match the specified languages
  expect_equal(sort(new_sheet$sheets$name), sort(test_lang_names))

  # Clean up: delete the created test sheet
  googledrive::drive_trash(file=test_ssheet_name)

})
