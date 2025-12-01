library(testthat)

# tests/testthat/test-get_CMEMS_ncdf.R

test_that("get_CMEMS_ncdf sets up venv on first run, logs in, and calls subset with correct args", {
  skip_if_not_installed("mockery")
  
  library(mockery)
  
  # Arrange: mock reticulate calls
  use_virtualenv_mock <- mock(
    structure("first-call-failed", class = "try-error"), # first call triggers setup path
    NULL                                                 # second call succeeds
  )
  virtualenv_create_mock <- mock(NULL)
  virtualenv_install_mock <- mock(NULL)
  
  # Capture subset call and create the output file as side-effect
  subset_called <- 0
  subset_args <- NULL
  subset_fun <- function(...) {
    subset_called <<- subset_called + 1
    subset_args <<- list(...)
    ofile <- file.path(subset_args$output_directory, subset_args$output_filename)
    dir.create(subset_args$output_directory, recursive = TRUE, showWarnings = FALSE)
    file.create(ofile)
    invisible(NULL)
  }
  
  # Mock login to assert it's called with credentials
  login_mock <- mock(NULL)
  
  # Mock import("copernicusmarine") to return our fake module
  import_mock <- function(module) {
    expect_equal(module, "copernicusmarine")
    list(
      login = function(...) login_mock(...),
      subset = subset_fun
    )
  }
  
  # Stub within function
  stub(get_CMEMS_ncdf, "reticulate::use_virtualenv", use_virtualenv_mock)
  stub(get_CMEMS_ncdf, "reticulate::virtualenv_create", virtualenv_create_mock)
  stub(get_CMEMS_ncdf, "reticulate::virtualenv_install", virtualenv_install_mock)
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
  
  # Assert: venv setup path taken
  expect_called(use_virtualenv_mock, 2)
  expect_called(virtualenv_create_mock, 1)
  expect_called(virtualenv_install_mock, 1)
  
  # Assert: login called with credentials
  expect_called(login_mock, 1)
  expect_args(login_mock, 1, "user", "pass")
  
  # Assert: subset called once with expected arguments and created the file
  expect_equal(subset_called, 1)
  expect_true(file.exists(out_nc))
  expect_identical(subset_args$dataset_id, default_dataset)
  expect_identical(subset_args$variables, vars)
})

test_that("get_CMEMS_ncdf skips login when credentials are NA and still calls subset", {
  skip_if_not_installed("mockery")
  
  library(mockery)
  
  # Mock a successful use_virtualenv (no venv creation/install)
  use_virtualenv_mock <- mock(NULL, cycle = TRUE)
  virtualenv_create_mock <- mock(NULL)
  virtualenv_install_mock <- mock(NULL)
  
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
    expect_equal(module, "copernicusmarine")
    list(
      login = function(...) login_mock(...),
      subset = subset_fun
    )
  }
  
  stub(get_CMEMS_ncdf, "reticulate::use_virtualenv", use_virtualenv_mock)
  stub(get_CMEMS_ncdf, "reticulate::virtualenv_create", virtualenv_create_mock)
  stub(get_CMEMS_ncdf, "reticulate::virtualenv_install", virtualenv_install_mock)
  stub(get_CMEMS_ncdf, "import", import_mock)
  
  out_nc <- tempfile(fileext = ".nc")
  get_CMEMS_ncdf(variables = list("thetao"), output_filename = out_nc)
  
  # No venv creation/install expected
  expect_called(use_virtualenv_mock, 2)
  expect_called(virtualenv_create_mock, 0)
  expect_called(virtualenv_install_mock, 0)
  
  # Login should be skipped
  expect_called(login_mock, 0)
  
  # Subset still called and file created
  expect_equal(subset_called, 1)
  expect_true(file.exists(out_nc))
})
