# tests/testthat/test-ea-classes.R

test_that("ea_data constructor and basic print/summary/plot work", {
  # Construct test data
  df <- data.frame(
    year = 2001:2005,
    value = c(1, 2, 3, 2, 1),
    low = c(0, 1, 2, 1, 0),
    high = c(2, 3, 4, 3, 2)
  )
  
  obj <- ea_data(
    data = df,
    value_col = "value",
    data_type = "Synthetic Test Index",
    region = "TestRegion",
    location_descriptor = "TestArea",
    units = "foo",
    species = "SpecX",
    source_citation = "TestSource"
  )
  
  expect_s3_class(obj, "ea_data")
  expect_true(is.data.frame(obj$data))
  expect_true(class(obj$meta)[1] == "list")
  expect_true("value" %in% names(obj$data))
  expect_equal(obj$meta$data_type, "Synthetic Test Index")
  
  # Print/summary should not error
  expect_output(print(obj), "Ecosystem Approach \\(EA\\) Data Object", fixed = FALSE)
  expect_output(summary(obj), "Summary of ea_data")
})

test_that("plot.ea_data supports all styles without error", {
  df <- data.frame(
    year = 2010:2014,
    value = c(2,4,6,3,5),
    low = c(1,2,5,1,3),
    high = c(3,6,7,5,8)
  )
  obj <- ea_data(
    data = df,
    value_col = "value",
    data_type = "biomass anomaly test data",
    region = "Test",
    location_descriptor = "Loc",
    units = "bar"
  )
  
  # Test all plot styles
  for (s in c("default", "ribbon", "plain", "anomaly", "biomass", "red_blue")) {
    expect_s3_class(
      plot(obj, style = s),
      "ggplot"
    )
  }
})

test_that("ea_data subsetting preserves class and meta", {
  df <- data.frame(
    year = 1:5,
    value = 10:14
  )
  obj <- ea_data(
    data = df,
    value_col = "value",
    data_type = "TestSubsetting",
    region = "A",
    location_descriptor = "B",
    units = "x"
  )
  sub <- obj[1:2, ]
  expect_s3_class(sub, "ea_data")
  expect_equal(sub$meta$data_type, obj$meta$data_type)
  expect_equal(nrow(sub$data), 2)
})

test_that("ea_st constructor and plot/print/summary work", {
  skip_if_not_installed("sf")
  library(sf)
  pts <- data.frame(x = 1:3, y = 1:3, mytemp = c(0.5, 1.5, 2.5))
  sfo <- st_as_sf(pts, coords = c("x", "y"), crs = 4326)
  est <- ea_st(
    data = sfo,
    value_col = "mytemp",
    data_type = "ToyTemp",
    region = "Tinyland",
    time_descriptor = "early-spring",
    units = "degC"
  )
  expect_s3_class(est, "ea_st")
  expect_true(is.list(est))
  expect_true(is.data.frame(est$data))
  expect_true("value" %in% names(est$data))
  expect_output(print(est), "Ecosystem Approach Spatio-Temporal")
  expect_output(summary(est), "Summary of ea_st Object")
  
  # All plot styles
  for (style in c("fill", "bubble")) {
    expect_warning(plot(est, style = style), "attribute variables are assumed to be spatially constant throughout all geometries")
  }
})

test_that("as_ea_data and as_ea_st adapters work on pacea-like objects", {
  # Simulate a pacea_index object using only attributes
  d <- data.frame(year=1:3, val=11:13, anom=101:103)
  attr(d, "long_name") <- "Fake PACEA Index"
  attr(d, "axis_name") <- "Another Index Name"
  class(d) <- c("pacea_index","tbl_df","tbl","data.frame")
  
  out <- as_ea_data(d, value = 'val')
  expect_s3_class(out, "ea_data")
  expect_equal(out$meta$data_type, "Fake PACEA Index")
  expect_true("value" %in% names(out$data))
  
  # Simulate basic sf conversion only if sf is available
  skip_if_not_installed("sf")
  library(sf)
  sf_obj <- st_as_sf(data.frame(x=1:2,y=1:2,temp=11:12), coords = c("x","y"), crs=4326)
  attr(sf_obj, "axis_name") <- "GridTest"
  attr(sf_obj, "units") <- "degC"
  class(sf_obj) <- c("marea_st", class(sf_obj))
  out_st <- as_ea_st(sf_obj, value_col = "temp")
  expect_s3_class(out_st, "ea_st")
  expect_true("value" %in% names(out_st$data))
})
