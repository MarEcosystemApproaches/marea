library(testthat)

test_that("get_CMEMS_ncdf logs in and calls subset with correct args", {
  skip_if_not_installed("mockery")
  library(mockery)

  # Capture subset call and create the output file as side-effect
  subset_called <- 0
  subset_args <- NULL
  subset_fun <- function(...) {
    subset_called <<- subset_called + 1
    subset_args <<- list(...)
    ofile <- file.path(
      subset_args$output_directory,
      subset_args$output_filename
    )
    dir.create(
      subset_args$output_directory,
      recursive = TRUE,
      showWarnings = FALSE
    )
    file.create(ofile)
    invisible(NULL)
  }

  # Mock login to assert it's called with credentials
  login_mock <- mock(NULL)

  # Mock import("copernicusmarine") to return our fake module
  import_mock <- function(module) {
    if (module == "copernicusmarine") {
      list(
        login = login_mock,
        subset = subset_fun
      )
    }
  }

  # Stub the actual functions your code calls
  stub(get_CMEMS_ncdf, "py_available", function(...) TRUE)
  stub(get_CMEMS_ncdf, "py_module_available", function(...) TRUE)
  stub(get_CMEMS_ncdf, "import", import_mock)

  # Act
  out_nc <- tempfile(fileext = ".nc")
  vars <- list("thetao")
  default_dataset <- "cmems_mod_glo_phy_my_0.083deg_P1M-m"

  get_CMEMS_ncdf(
    username = "user",
    password = "pass",
    variables = vars,
    output_filename = out_nc
  )

  # Assert: login called with credentials
  expect_called(login_mock, 1)
  expect_args(login_mock, 1, "user", "pass", overwrite = TRUE)

  # Assert: subset called once with expected arguments and created the file
  expect_equal(subset_called, 1)
  expect_true(file.exists(out_nc))
  expect_identical(subset_args$dataset_id, default_dataset)
  expect_identical(subset_args$variables, vars)
})

test_that("get_CMEMS_ncdf skips login when credentials are NA and still calls subset", {
  skip_if_not_installed("mockery")
  library(mockery)

  subset_called <- 0
  subset_fun <- function(...) {
    subset_called <<- subset_called + 1
    args <- list(...)
    ofile <- file.path(args$output_directory, args$output_filename)
    dir.create(args$output_directory, recursive = TRUE, showWarnings = FALSE)
    file.create(ofile)
    invisible(NULL)
  }

  login_mock <- mock(NULL)

  import_mock <- function(module) {
    if (module == "copernicusmarine") {
      list(
        login = login_mock,
        subset = subset_fun
      )
    }
  }

  stub(get_CMEMS_ncdf, "py_available", function(...) TRUE)
  stub(get_CMEMS_ncdf, "py_module_available", function(...) TRUE)
  stub(get_CMEMS_ncdf, "import", import_mock)

  out_nc <- tempfile(fileext = ".nc")
  get_CMEMS_ncdf(variables = list("thetao"), output_filename = out_nc)

  # Login should be skipped
  expect_called(login_mock, 0)

  # Subset still called and file created
  expect_equal(subset_called, 1)
  expect_true(file.exists(out_nc))
})

test_that("get_CMEMS_ncdf handles character variables correctly", {
  skip_if_not_installed("mockery")
  library(mockery)

  subset_called <- 0
  subset_args <- NULL
  subset_fun <- function(...) {
    subset_called <<- subset_called + 1
    subset_args <<- list(...)
    args <- subset_args
    ofile <- file.path(args$output_directory, args$output_filename)
    dir.create(args$output_directory, recursive = TRUE, showWarnings = FALSE)
    file.create(ofile)
    invisible(NULL)
  }

  import_mock <- function(module) {
    if (module == "copernicusmarine") {
      list(
        login = mock(NULL),
        subset = subset_fun
      )
    }
  }

  stub(get_CMEMS_ncdf, "py_available", function(...) TRUE)
  stub(get_CMEMS_ncdf, "py_module_available", function(...) TRUE)
  stub(get_CMEMS_ncdf, "import", import_mock)

  out_nc <- tempfile(fileext = ".nc")

  # Pass character vector instead of list
  get_CMEMS_ncdf(
    variables = c("thetao", "so"),
    output_filename = out_nc
  )

  # Should be converted to list
  expect_equal(subset_called, 1)
  expect_equal(subset_args$variables, list("thetao", "so"))
})

test_that("get_CMEMS_ncdf throws error when variables is not character or list", {
  skip_if_not_installed("mockery")
  library(mockery)

  stub(get_CMEMS_ncdf, "py_available", function(...) TRUE)
  stub(get_CMEMS_ncdf, "py_module_available", function(...) TRUE)

  expect_error(
    get_CMEMS_ncdf(variables = 123),
    "variables must be a character vector or list"
  )
})

test_that("get_CMEMS_ncdf throws error when Python is not available", {
  skip_if_not_installed("mockery")
  library(mockery)

  stub(get_CMEMS_ncdf, "py_available", function(...) FALSE)

  expect_error(
    get_CMEMS_ncdf(variables = "thetao"),
    "Python is not installed"
  )
})
