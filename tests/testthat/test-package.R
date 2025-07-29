library(testthat)

testthat::test_that("marea", {
  # Load the package
  library(marea)

  # Test if the package loads correctly
  expect_true("marea" %in% rownames(installed.packages()))

  # Test if a specific function exists
  expect_true(exists("marea_metadata", where = "package:marea"))


})
