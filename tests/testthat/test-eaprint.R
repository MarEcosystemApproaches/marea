# tests/testthat/test-ea.print.R

test_that("ea.print prints ea_data summary and returns invisibly", {
  testthat::local_reproducible_output()
  
  # Ensure minimal S4 classes exist for testing
  if (!methods::isClass("ea_data")) {
    methods::setClass("ea_data", representation(meta = "list", data = "data.frame"))
  }
  
  meta <- list(
    data_type = "biological",
    value_col = 'value',
    species = "Cod",
    location_descriptor = "Area51",
    region = "ATL",
    units = "kg"
  )
  df <- data.frame(
    year = c(1990, 1995, 2000, NA),
    sp_value = c(1, 2, 3, NA_real_)
  )
  obj <- methods::new("ea_data", meta = meta, data = df)
  
  vis <- NULL
  out <- testthat::capture_output(vis <- withVisible(ea.print(obj)))
  
  # Output contains expected summary lines
  expect_match(out, "Ecosystem Approach \\(EA\\) Data Object")
  expect_match(out, "Class:\\s+ea_data")
  expect_match(out, "Data Type:\\s+biological")
  expect_match(out, "Species:\\s+Cod")
  expect_match(out, "ATL  Region")
  expect_match(out, "Time Range:\\s+1990\\s+-\\s+2000")
  expect_match(out, "Units:\\s+kg")
  expect_match(out, "Data Preview:")
  # Preview should include column names from data
  expect_match(out, "year")
  expect_match(out, "sp_value")
  
  # Returned value is the object and is invisible
  expect_false(vis$visible)
  expect_identical(vis$value, obj)
})

test_that("ea.print for ea_data omits Species line when species is NA", {
  testthat::local_reproducible_output()
  
  if (!methods::isClass("ea_data")) {
    methods::setClass("ea_data", representation(meta = "list", data = "data.frame"))
  }
  
  meta <- list(
    data_type = "biological",
    value_col = 'sp',
    species = NA_character_,
    location_Descriptor = "Somewhere",
    region = "PAC",
    units = "counts"
  )
  df <- data.frame(
    year = c(2001, 2002, NA),
    sp_value = c(10, 20, 30)
  )
  obj <- methods::new("ea_data", meta = meta, data = df)
  
  out <- testthat::capture_output(ea.print(obj))
  expect_match(out, "Data Type:\\s+biological")
  expect_false(grepl("\\bSpecies\\s*:", out))
})

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

test_that("ea.print prints ea_spatial summary and returns invisibly", {
  testthat::local_reproducible_output()
  
  if (!methods::isClass("ea_spatial")) {
    methods::setClass("ea_spatial", representation(meta = "list", data = "data.frame"))
  }
  
  obj <- create_test_sf()
  
  
  vis <- NULL
  out <- testthat::capture_output(vis <- withVisible(ea.print(obj)))
  
  expect_match(out, "Ecosystem Approach Spatio-Temporal \\(ea_spatial\\) Object")
  expect_match(out, "Temperature")
  expect_match(out, "Time:\\s+Annual")
  expect_match(out, "Region:")
  expect_match(out, "Units:\\s+degC \\(in 'value' column, originally 'temp_val'\\)")
  # Printed data should include columns
  expect_match(out, "id")
  expect_match(out, "value")
  expect_match(out, "geometry")
  
  expect_false(vis$visible)
  expect_identical(vis$value, obj)
})

test_that("ea.print errors for unsupported classes", {
  expect_error(
    ea.print(list(meta = list(), data = data.frame())),
    "Unknown class for print",
    fixed = TRUE
  )
})
