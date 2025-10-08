# tests/testthat/test-ea.summary.R
library(testthat)

# Helper function to create a basic ea_data object for tests
create_test_ea_data <- function(years = 2000:2005, values = rnorm(6),
                                value_col_name = "temp_c",
                                data_type = "temperature",
                                region = "Scotian Shelf",
                                location_descriptor = "bottom",
                                units = "°C",
                                extra_cols = NULL) {
  df <- data.frame(year = years)
  df[[value_col_name]] <- values
  if (!is.null(extra_cols)) {
    for (col_name in names(extra_cols)) {
      df[[col_name]] <- extra_cols[[col_name]]
    }
  }
  
  ea_data(
    data = df,
    value_col = value_col_name,
    data_type = data_type,
    region = region,
    location_descriptor = location_descriptor,
    units = units
  )
}
test_that("ea.summary prints ea_data summary and returns invisible structured result", {
  testthat::local_reproducible_output()
  
  # Define a minimal ea_data class if not provided by the package
  if (!methods::isClass("ea_data")) {
    methods::setClass("ea_data", representation(meta = "list", data = "data.frame"))
  }
  
  obj <- create_test_ea_data(
    years = c(1990, 1995, 2000, NA),
    values = c(5.5, 6.1, 5.8, 6.0),
    value_col_name = "values",
    data_type = "biological",
    region = "ATL",
    location_descriptor = "Area51",
    units = "kg",
    extra_cols = list(species = c("Cod", "Cod", "Cod", "Cod"))
  )
  
  vis <- NULL
  out <- testthat::capture_output(vis <- withVisible(ea.summary(obj)))
  
  # Key lines present
  expect_match(out, "--- Summary of ea_data ---", fixed = TRUE)
  expect_match(out, "Data Type:\\s+biological")
  expect_match(out, "Location:\\s+Area51")
  expect_match(out, "Region:\\s+ATL")
  expect_match(out, "Source:")
  expect_match(out, "Time range:\\s+1990\\s+to\\s+2000")
  expect_match(out, "Number of observations:\\s+4")
  expect_match(out, "Summary of 'value' column \\(Units: kg\\):")
  
  # Return value: invisible, and structured correctly
  expect_false(vis$visible)
  expect_type(vis$value, "list")
})

test_that("ea.summary for ea_data omits Species when missing", {
  testthat::local_reproducible_output()
  
  if (!methods::isClass("ea_data")) {
    methods::setClass("ea_data", representation(meta = "list", data = "data.frame"))
  }
  
  obj <- create_test_ea_data(
    years = c(1990, 1995, 2000, NA),
    values = c(5.5, 6.1, 5.8, 6.0),
    value_col_name = "values",
    data_type = "biological",
    region = "ATL",
    location_descriptor = "Area51",
    units = "kg"  )
  out <- testthat::capture_output(ea.summary(obj))
  expect_match(out, "Data Type:\\s+biological")
  expect_false(grepl("\\bSpecies\\s*:", out))
})
# Helper function to create a basic sf object
create_test_sf <- function(n = 3, value_col_name = "temp_val",
                           data_type = "Temperature",
                           region = "Test Region",
                           time_descriptor = "Annual Avg",
                           units = "degC",
                           extra_cols = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    testthat::skip("sf package not installed")
  }
  pts <- lapply(1:n, function(i) sf::st_point(c(-63 + i, 44 + i)))
  df <- data.frame(id = 1:n)
  df[[value_col_name]] <- rnorm(n, mean = 15, sd = 2)
  if (!is.null(extra_cols)) {
    for (col_name in names(extra_cols)) {
      df[[col_name]] <- extra_cols[[col_name]]
    }
  }
  sf_obj <- sf::st_sf(df, geometry = sf::st_sfc(pts, crs = 4326))
  
  ea_spatial(
    data = sf_obj,
    value_col = value_col_name,
    data_type = data_type,
    region = region,
    time_descriptor = time_descriptor,
    units = units
  )
}
test_that("ea.summary prints ea_spatial summary, geometry, time periods, and returns invisible structured result", {
  skip_if_not_installed("sf")
  testthat::local_reproducible_output()
  
  # Allow sf objects in the data slot when defining our minimal class
  if (!methods::isClass("ea_spatial")) {
    methods::setClass("ea_spatial", representation(meta = "list", data = "ANY"))
  }
  
 obj <- create_test_sf(
    n = 5,
    value_col_name = "value",
    data_type = "temperature",
    region = "ATL",
    time_descriptor = "1990-1991",
    units = "degC",
    extra_cols = list(time_descriptor = c(1990, 1990, 1991, 1991, 1991))
  )
  vis <- NULL
  out <- testthat::capture_output(vis <- withVisible(ea.summary(obj)))
  
  # Header and metadata
  expect_match(out, "--- Summary of ea_spatial Object ---", fixed = TRUE)
  expect_match(out, "Data Type:\\s+temperature")
  expect_match(out, "Region:\\s+ATL")
  expect_match(out, "Time:\\s+1990-1991")
  expect_match(out, "Source:")
  
  # Geometry and time periods
  expect_true(grepl("Spatial Information \\(from sf\\):", out))
  expect_true(grepl("Geometry set", out))  # sf geometry summary line
  expect_match(out, "Time periods:\\s+1990, 1991")
  expect_match(out, "Number of time periods:\\s+2")
  
  # Value summary
  expect_match(out, "Summary of 'value' column \\(Units: degC\\):")
  
  # Return value
  expect_false(vis$visible)
  expect_type(vis$value, "list")
})

test_that("ea.summary errors for unsupported classes", {
  expect_error(
    ea.summary(list(meta = list(), data = data.frame())),
    "Unknown class for summary",
    fixed = FALSE
  )
})
