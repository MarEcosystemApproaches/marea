## ----setup, echo = FALSE, warnings = FALSE, message = FALSE-------------------
library(marea)
library(tidyverse)

## -----------------------------------------------------------------------------
# Load grey seal abundance data
data("grey_seals")

# Print object summary (metadata, structure, preview)
ea.print(grey_seals)

## -----------------------------------------------------------------------------
ea.summary(grey_seals)

## -----------------------------------------------------------------------------
# plot(grey_seals)

## -----------------------------------------------------------------------------
# plot(grey_seals, style = "ribbon")

## -----------------------------------------------------------------------------
data("glorys_bottom_temperature")
ea.summary(glorys_bottom_temperature)

## -----------------------------------------------------------------------------
# plot(glorys_bottom_temperature)

## -----------------------------------------------------------------------------
library(pacea)
data(npgo) # Example: North Pacific Gyre Oscillation
marea_npgo <- as_ea_data(x = npgo, 
                         value_col = c("anomaly"))
class(marea_npgo)
ea.print(marea_npgo)

## -----------------------------------------------------------------------------
library(pacea)
data("hake_total_biomass_age_1_2025")

hake_biomass_ea <- as_ea_data(
  hake_total_biomass_age_1_2025,
  value_col = c("median"),
  source = 'pacea',
  time_descriptor = "2025",
  details = 'Converted using marea::as_ea_data() July 2025'
)
print(hake_biomass_ea)


## -----------------------------------------------------------------------------
# Create a simple temperature dataset
temp_data <- data.frame(
  year = 2010:2020,
  avg_temp = c(8.2, 8.5, 8.1, 8.9, 9.2, 8.8, 9.1, 8.7, 9.0, 8.6, 9.3),
  station = "Halifax-2"
)

# Create ea_data object
halifax_temp <- ea_data(
  data = temp_data,
  value_col = "avg_temp",  # Specify which column contains the values
  data_type = "Bottom Temperature",
  region = "Maritimes",
  location_descriptor = "Halifax Line Station 2",
  units = "°C",
  source_citation = "DFO Maritimes Region Monitoring Program"
)

ea.print(halifax_temp)

## -----------------------------------------------------------------------------
# Fisheries catch data with uncertainty
catch_data <- data.frame(
  year = 2015:2022,
  landings = c(1250, 1180, 1320, 1450, 1280, 1390, 1220, 1310),
  vessel_count = c(45, 42, 48, 51, 46, 49, 44, 47)
)

# Create object with additional metadata
lobster_catch <- ea_data(
  data = catch_data,
  value_col = "landings",
  data_type = "Commercial Landings",
  region = "Maritimes",
  location_descriptor = "LFA 34",
  units = "tonnes",
  species = "American Lobster",
  source_citation = "DFO INTERNAL",
  # Additional custom metadata
  fishing_season = "November-May",
  assessment_year = 2023,
  stock_status = "Healthy"
)

ea.print(lobster_catch)
ea.summary(lobster_catch)

## -----------------------------------------------------------------------------
library(sf)

# Create a simple spatial grid
grid_points <- expand.grid(
  lon = seq(-66, -64, by = 0.5),
  lat = seq(43, 45, by = 0.5)
)

# Convert to sf object
spatial_data <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326)

# Add some simulated chlorophyll data
spatial_data$chl_concentration <- runif(nrow(spatial_data), 0.5, 3.2)
spatial_data$depth_zone <- "surface"

# Create ea_st object
chl_spatial <- ea_spatial(
  data = spatial_data,
  value_col = "chl_concentration",
  data_type = "Chlorophyll-a Concentration",
  region = "Maritimes",
  time_descriptor = "July 2023 Survey",
  units = "mg/m³",
  source_citation = "DFO Ecosystem Survey Program"
)

ea.print(chl_spatial)

## -----------------------------------------------------------------------------
# Use the halifax_temp object created earlier
# Access the entire metadata list
halifax_temp@meta

# Access the entire data tibble
halifax_temp@data

## -----------------------------------------------------------------------------
# Get the entire meta list (same as @meta)
halifax_temp[["meta"]]

# Get a specific metadata item
halifax_temp[["region"]]

# Get the entire data tibble (same as @data)
halifax_temp[["data"]]

# Get a specific data column (vector)
halifax_temp[["year"]]

## -----------------------------------------------------------------------------
citation("marea")

