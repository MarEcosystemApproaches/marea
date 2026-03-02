# tests for assorted functions in marea

library(testthat)

test_that("marea_citation_reminder emits messages and prints a citation", {
  testthat::local_reproducible_output()
  
  # Messages are sent to stderr
  msgs <- testthat::capture_messages(marea_citation_reminder())
  expect_true(any(grepl("Thank you for using marea!", msgs)))
  expect_true(any(grepl("Please cite this package in your work:", msgs)))

  # The citation itself is printed to stdout; ensure something sensible is printed
  expect_output(suppressMessages(marea_citation_reminder()), "marea")
})

test_that("cite_marea returns a citation object by default", {
  testthat::local_reproducible_output()
  
  expect_silent(cit <- cite_marea())
  expect_s3_class(cit, "citation")
  expect_equal(cit, citation("marea"))
})

test_that("cite_marea returns BibTeX when requested", {
  testthat::local_reproducible_output()
  
  expect_silent(bib <- cite_marea(TRUE))
  expect_true(is.character(bib))
  expect_s3_class(bib, "Bibtex")
  txt <- paste(bib, collapse = "\n")
  expect_match(txt, "@", fixed = TRUE)
  expect_match(txt, "marea")
})
