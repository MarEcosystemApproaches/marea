# Tests for time series plotting functions in marea
#
# These tests check that plotting functions for indices and AZMP bottom temperature
# behave as expected, including error handling and various plotting options.
# For more comprehensive examples, see the pacea package.

test_that("index plotting: stopifnot error handling works", {
  expect_error(plot(oni, value = "foo"))
})

test_that("index plotting works with various options", {
  expect_invisible(plot(oni, style = "red_blue"))

  expect_invisible(plot(oni,
    xlim = c(lubridate::dmy(01011950), lubridate::dmy(01012040))
  )) # expand x-axis

  expect_invisible(plot(oni, type = "o")) # pass type to plot()
  expect_invisible(plot(oni, main = "My nice plot")) # pass main to plot()

  # The following tests are commented out because they do not currently work in marea,
  # but do work in pacea. Uncomment and adapt as needed if/when functionality is added.

  # expect_invisible(plot(oni,
  #   event_years = c(2007, 2008, 2010),
  #   value = "value",
  #   style = "plain"
  # ))

  # expect_invisible(plot(oni,
  #   event_lub = c(lubridate::dmy(02081972), lubridate::dmy(31121999))
  # ))

  suppressWarnings(expect_warning(
    plot(oni, not_an_option = "My nice plot"),
    regexp = NA # Accept any warning, as this is a nonsensical ... option
  ))

  expect_invisible(plot(oni, style = "goa")) # not implemented yet, should not error
})

test_that("Plotting AZMP Bottom Temperature", {
  expect_invisible(plot(azmp_bottom_temperature, value = "mean"))

  expect_invisible(plot(azmp_bottom_temperature, value = "mean", region = c("4X", "4W")))

  expect_error(
    plot(azmp_bottom_temperature, value = "mean", region = "hello"),
    "region must be in Cabot Strait,E Georges Bank,Emerald Basin,Georges Basin,Lurcher Shoal,Misaine Bank,4Vn,4Vs,4W,4X"
  )
})
