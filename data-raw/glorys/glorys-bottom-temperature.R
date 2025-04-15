# Including example GLORYS bottom temperature results from Adam; Remi making a
# more general function to extract more.

# 12 months of bottom temperature for marea is 288kb, so saving as object in the
# package.


# Adapting from pacea data-raw/roms/roms-data-interpolation-full.R.
# See that for further useful things, e.g.:
#  - parallel code if doing multiple variables that take a long time
#  - saving files outside of the package if they are 'too big'
#  - masking using coastline, bc_coast
# deleting such things deleting such things for this example to keep it simpler.

library(devtools)
library(dplyr)
library(terra)
library(gstat)
library(sf)
library(stars)
library(ncdf4)
library(ggplot2)
library(concaveman)
library(parallel)
library(foreach)
library(pacea)

sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

load_all()

marea_dir <- here::here()   # Will give local marea/

temp_file <- tempfile(fileext = ".nc")

get_CMEMS_ncdf(
  output_filename = temp_file,
  variables=list("bottomT"),
  start_datetime="1993-01-01T00:00:00",
  end_datetime="2021-06-01T00:00:00")

full <- stars::read_ncdf(temp_file, var = "bottomT")

full_sf <- st_as_sf(full)  %>%
  units::drop_units()

old_names <- names(full_sf)

new_names <- gsub("-", "_", stringr::str_sub(old_names[-length(full_sf)], start = 1, end = 7)) %>%
  sub("_0", "_", .)

names(full_sf) <- c(new_names, "geometry")

glorys_bottom_temperature <- full_sf %>%
  sf::st_drop_geometry() %>%
  round(digits = 6) %>%
  sf::st_as_sf(geometry = sf::st_geometry(full_sf))

class(glorys_bottom_temperature) <- c("marea_st", "sf", "tbl_df", "tbl", "data.frame")

attr(glorys_bottom_temperature, "units") <- "Temperature (\u00B0C)"

plot(glorys_bottom_temperature, year=2020, month=1:12)

usethis::use_data(glorys_bottom_temperature,
                  overwrite = TRUE)