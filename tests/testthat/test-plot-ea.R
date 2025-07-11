test_that("plot.ea_data produces valid ggplot objects for all styles", {
  df <- data.frame(
    year = 2010:2014,
    value = c(1,2,3,2,1),
    low = c(0,1,1,0,0),
    high = c(2,3,5,3,2)
  )
  obj <- ea_data(
    data = df,
    value_col = "value",
    data_type = "PlotTest",
    region = "Caravan",
    location_descriptor = "TestLocation",
    units = "kg"
  )
  
  # Default style
  p1 <- plot(obj, style = "default")
  expect_s3_class(p1, "ggplot")
  
  # Ribbon style (should succeed, relies on low/high columns)
  p2 <- plot(obj, style = "ribbon")
  expect_s3_class(p2, "ggplot")
  
  # pacea style (requires low/high)
  p3 <- plot(obj, style = "pacea")
  expect_s3_class(p3, "ggplot")
  
  # Plain style (just a line)
  p4 <- plot(obj, style = "plain")
  expect_s3_class(p4, "ggplot")
  
  # Red-blue
  p5 <- plot(obj, style = "red_blue")
  expect_s3_class(p5, "ggplot")
  
  # Should fail gracefully if low/high removed in ribbon style
  obj_no_ci <- obj
  obj_no_ci$data <- obj_no_ci$data[, !(names(obj_no_ci$data) %in% c("low", "high"))]
  expect_error(plot(obj_no_ci, style = "ribbon"))
})

test_that("plot.ea_st produces valid ggplot objects for all spatial styles", {
  skip_if_not_installed("sf")
  library(sf)
  sfdat <- st_as_sf(data.frame(x=1:3, y=1:3, val=c(10,20,30)), coords=c("x","y"), crs=4326)
  est <- ea_st(
    data = sfdat,
    value_col = "val",
    data_type = "PlotTestSpatial",
    region = "Nowhere",
    time_descriptor = "FakeSlice",
    units = "degB"
  )
  styles <- c("fill", "bubble")
  for (sm in styles) {
    expect_s3_class(plot(est, style = sm), "ggplot")
  }
})

test_that("plot.ea_data passes ... to ggplot geoms", {
  df <- data.frame(year=2000:2002, value=1:3)
  obj <- ea_data(
    data = df,
    value_col = "value",
    data_type = "GeomArgsTest",
    region = "Testreg",
    location_descriptor = "Locy",
    units = "Zol"
  )
  # Custom color/size
  p <- plot(obj, color = "orange")
  expect_s3_class(p, "ggplot")
})

# test that summary, print and plot all interact without side effects
test_that("plot, print, summary play nicely together", {
  df <- data.frame(year=1990:1994, value=10:14)
  obj <- ea_data(
    data = df, value_col="value",
    data_type="Test", region="R", location_descriptor="L", units="u"
  )
  expect_output(print(obj), "Ecosystem Approach \\(EA\\) Data Object", fixed = FALSE)
  expect_output(summary(obj), "Summary of ea_data")
  expect_s3_class(plot(obj), "ggplot")
})
