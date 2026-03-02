devtools::load_all()
library(dplyr)
library(readr)
library(tibble)
library(marea)

# Helper function
make_ea_index <- function(
    df, value_col, data_type, region, location, units, source, species = NA_character_, ...
) {
  ea_data(
    data = df,
    value_col = value_col,
    data_type = data_type,
    region = region,
    location_descriptor = location,
    units = units,
    species = species,
    source_citation = source, 
    ...
  )
}

# ---- NAO ----
remotes::install_github("casaultb/azmpdata")
library(azmpdata)

nao_new <- tibble(
  year = azmpdata::Derived_Annual_Broadscale$year,
  anomaly = azmpdata::Derived_Annual_Broadscale$north_atlantic_oscillation
) %>%
  filter(!is.na(anomaly))

nao <- make_ea_index(
  df = nao_new,
  value_col = "anomaly",
  data_type = "North Atlantic Oscillation Index",
  region = "North Atlantic",
  location = "Azores–Iceland SLP difference",
  units = "",  # Standardized index
  source = "NOAA NCEP via azmpdata; https://www.ncei.noaa.gov/access/monitoring/nao/"
)
usethis::use_data(nao, overwrite = TRUE)

# ---- AZMP BOTTOM TEMPERATURE ----
azmp_bottom_temperature_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$temperature_at_sea_floor
) %>%
  filter(!is.na(mean))

azmp_bottom_temperature <- make_ea_index(
  df = azmp_bottom_temperature_new,
  value_col = "mean",
  data_type = "Bottom Temperature",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea floor mean temperature",
  units = "deg C",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata"
)
usethis::use_data(azmp_bottom_temperature, overwrite = TRUE)

# ---- AZMP SURFACE TEMPERATURE ----
azmp_surface_temperature_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$sea_temperature_0
) %>%
  filter(!is.na(mean))

azmp_surface_temperature <- make_ea_index(
  df = azmp_surface_temperature_new,
  value_col = "mean",
  data_type = "Surface Temperature",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea surface mean temperature",
  units = "deg C",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata"
)
usethis::use_data(azmp_surface_temperature, overwrite = TRUE)

# ---- AZMP SURFACE TEMPERATURE FROM SATELLITE ----
azmp_satellite_temperature_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$sea_surface_temperature_from_satellite
) %>%
  filter(!is.na(mean))
azmp_satellite_temperature <- make_ea_index(
  df = azmp_satellite_temperature_new,
  value_col = "mean",
  data_type = "Surface Temperature from Satellite",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea surface mean temperature from satellite",
  units = "deg C",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata"
)
usethis::use_data(azmp_satellite_temperature, overwrite = TRUE)

# ---- AZMP STRATIFICATION ----

azmp_stratification_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$density_gradient_0_50
) %>%
  filter(!is.na(mean))

azmp_stratification <- make_ea_index(
  df = azmp_stratification_new,
  value_col = "mean",
  data_type = "Stratification Index",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO stratification index",
  units = "kg/m^3 per meter",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata",
  details = "An index of stratification, measured as the density difference between 0 and 50 metres"
)
usethis::use_data(azmp_stratification, overwrite = TRUE)


# ---- AZMP SALINITY ----
azmp_salinity_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$salinity_0
) %>%
  filter(!is.na(mean))

azmp_salinity <- make_ea_index(
  df = azmp_salinity_new,
  value_col = "mean",
  data_type = "Surface Salinity",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea surface mean salinity",
  units = "PSU",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata"
)
usethis::use_data(azmp_salinity, overwrite = TRUE)
  
  