
# Development Sheet ID - Accessible to anyone (VIEW RIGHTS)
test_sheet_id <- "15X4NWssFV83h5ZdKa80XUfWxgsFq-IJyr_7RrhfUHpI"
#Pull once outside of testing
test_sheet <- get_tdb_data(ss = test_sheet_id)

# Test 1: Check if the function returns a named list of the expected length
test_that("get_tdb_data returns a named list of the expected length", {
  expect_type(test_sheet, "list")
  expect_named(test_sheet)
})

# Test 2: Check if the function returns a named list containing data.tables
test_that("get_tdb_data returns a named list containing data.tables", {
  expect_true(all(sapply(test_sheet, is.data.table)))
})

# Test 3: Check if the function returns the correct sheets when provided with specific sheet names
test_that("get_tdb_data returns the correct sheets", {
  specific_sheets <- c("German", "French")
  expect_equal(names(test_sheet), specific_sheets)
})

# Test 4: Check if the function returns an error when provided with an invalid Google Sheet ID
test_that("get_tdb_data returns an error with an invalid Google Sheet ID", {
  expect_error(get_tdb_data(ss = "invalid_id"))
})

# Test 5: Check if the function returns an error when provided with non-existent sheet names
test_that("get_tdb_data returns an error with non-existent sheet names", {
  expect_error(get_tdb_data(ss = test_sheet_id, sheets = c("non_existent_sheet")))
})
