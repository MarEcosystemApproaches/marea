# tests/testthat/test-marea_metadata.R
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
test_that("marea_metadata collects dataset metadata across classes", {
  skip_if_not_installed("mockery")
  library(mockery)
  
  # Define a minimal ea_data S4 class if not present
  if (!methods::isClass("ea_data")) {
    methods::setClass("ea_data", representation(meta = "list", data = "data.frame"))
  }
  
  # Create fake datasets covering all branches:
  # 1) ea_data S4 with slots and year column
  ds1 <- create_test_ea_data(
    years = 1990:1995,
    values = 1:6,
    value_col_name = "value",
    data_type = "biological",
    region = "ATL",
    location_descriptor = "Area51",
    units = "kg",
    extra_cols = list(year = 1990:1995)
  ) 
  # 2) ea_spatial-like: regular data.frame with class "ea_spatial" and meta attribute
  ds2 <- create_test_sf()
  
  # 3) Other data.frame with Year column and attrs for region and source
  ds3 <- data.frame(Year = c(1999, 2001), value = c(10, 20))
  attr(ds3, "region") <- "NE"
  attr(ds3, "source") <- "my-source"
  
  # 4) Other data.frame with no time/attrs to trigger Unknowns
  ds4 <- data.frame(foo = 1:2)
  
  # Mock data() to return our dataset names
  data_mock <- function(package) {
    expect_equal(package, "marea")
    list(results = data.frame(Item = c("d1", "d2", "d3", "d4"), stringsAsFactors = FALSE))
  }
  
  # Mock get() to return each dataset by name
  get_mock <- function(x, envir) {
    switch(x,
           d1 = ds1,
           d2 = ds2,
           d3 = ds3,
           d4 = ds4,
           stop("unexpected dataset name in test: ", x)
    )
  }
  
  # Stub the functions used inside marea_metadata()
  stub(marea_metadata, "data", data_mock)
  stub(marea_metadata, "get",  get_mock)
  
  # Act
  md <- marea_metadata()
  
  # Basic structure
  expect_s3_class(md, "data.frame")
  expect_identical(names(md), c("Dataset", "Region", "TimeSpan", "Source"))
  expect_identical(md$Dataset, c("d1", "d2", "d3", "d4"))
  
  # d1: ea_data branch
  r1 <- md[md$Dataset == "d1", , drop = FALSE]
  expect_identical(r1$Region,   "ATL")
  expect_identical(r1$TimeSpan, "1990-1995")
  expect_identical(r1$Source,   "No citation provided")
  
  # d2: ea_spatial branch with Date range and meta attribute
  r2 <- md[md$Dataset == "d2", , drop = FALSE]
  expect_identical(r2$Region,   "Test Region")
  expect_identical(r2$TimeSpan, "Unknown")
  expect_identical(r2$Source,   "No citation provided")
  
  # d3: other data.frame branch with Year column and source attr
  r3 <- md[md$Dataset == "d3", , drop = FALSE]
  expect_identical(r3$Region,   "NE")
  expect_identical(r3$TimeSpan, "1999-2001")
  expect_identical(r3$Source,   "my-source")
  
  # d4: other data.frame with no time/attrs -> Unknowns
  r4 <- md[md$Dataset == "d4", , drop = FALSE]
  expect_identical(r4$Region,   "Unknown")
  expect_identical(r4$TimeSpan, "Unknown")
  expect_identical(r4$Source,   "Unknown")
})
