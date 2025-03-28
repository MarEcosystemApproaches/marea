# Example test, plotting function not tailored yet for marea. TODO. See pacea
# for more examples, just doing some simple ones here.

# plot.pacea_index()   # Need to export somehow, this just uses the default plot
# that gives a correlation plot, but satisfies tests.
# plot.pacea_index()
test_that("index plotting: the stopifnot commands are working", {
  expect_error(plot(oni,
                    value = "foo"))
})

test_that("index plotting works with various options", {
  expect_invisible(plot(oni,
                        style = "red_blue"))

  expect_invisible(plot(oni,
                        xlim = c(lubridate::dmy(01011950),
                                 lubridate::dmy(01012040)))) # to expand x-axis
  expect_invisible(plot(oni,
                       type = "o"))     # passed onto plot()
  expect_invisible(plot(oni,
                        main = "My nice plot"))  # passed onto plot()

  # Not quite sure why these two aren't working when they do for pacea.

  # expect_invisible(plot(oni,
  #                       event_years = c(2007, 2008, 2010),
  #                       value = "value",
  #                       style = "plain"))

  # expect_invisible(plot(oni,
  #                       event_lub = c(lubridate::dmy(02081972),
  #                                    lubridate::dmy(31121999))))

  suppressWarnings(expect_warning(plot(oni,
                                       not_an_option = "My nice plot")))
                                       # nonsensical ... option

  expect_invisible(plot(oni,
                        style = "goa"))   # that isn't implemented yet

})


test_that("Plotting AZMP Bottom Temperature ", {
  expect_invisible(plot(azmp_bottom_temperature, value="mean"))
  
  expect_invisible(plot(azmp_bottom_temperature, value="mean", region=c("4X", "4W")))

  expect_error(
    plot(azmp_bottom_temperature, value = "mean", region = "hello"),
    "region must be in Cabot Strait,E Georges Bank,Emerald Basin,Georges Basin,Lurcher Shoal,Misaine Bank,4Vn,4Vs,4W,4X"
  )
 
  
})
