# ---- GLORYS Bottom Temperature ----

library(dplyr)
library(stars)
library(sf)
library(reticulate)
library(ncdf4)

# Set up parameters for GLORYS bottom temperature download
username <- Sys.getenv("CMEMS_USERNAME")  # Set these as environment variables
password <- Sys.getenv("CMEMS_PASSWORD")  # or replace with actual credentials

# Download GLORYS bottom temperature data
download_glorys_data <- function() {
  # Set up Python environment for Copernicus Marine
  pythonenv <- try(reticulate::use_virtualenv("CopernicusMarine", required = TRUE))
  
  if (inherits(pythonenv, "try-error")) {
    reticulate::virtualenv_create(envname = "CopernicusMarine")
    reticulate::virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
  }
  reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
  cmt <- reticulate::import("copernicusmarine")
  
  # Login if credentials provided
  if (!is.na(username) && !is.na(password)) {
    cmt$login(username, password)
  }
  
  # Create temporary file for download
  temp_nc <- tempfile(fileext = ".nc")
  
  # Download data - using 2018 as example, adjust dates as needed
  cmt$subset(
    dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
    variables = list("bottomT"),
    minimum_longitude = -67.74250,
    maximum_longitude = -54.90132,
    minimum_latitude = 40.04343,
    maximum_latitude = 47.83333,
    start_datetime = "2018-01-01T00:00:00",
    end_datetime = "2018-12-31T23:59:59",
    output_directory = dirname(temp_nc),
    output_filename = basename(temp_nc)
  )
  
  return(temp_nc)
}

# Download the data
nc_file <- download_glorys_data()

# Process the netCDF file
sf_use_s2(FALSE)  # remove spherical geometry (s2) for sf operations

# Read the netCDF file
full <- stars::read_ncdf(nc_file, var = "bottomT")

# Convert to sf and drop units
full_sf <- st_as_sf(full) %>%
  units::drop_units()

# Clean up column names (remove dashes, standardize date format)
old_names <- names(full_sf)
new_names <- gsub("-", "_", 
                  stringr::str_sub(old_names[-length(full_sf)], start = 1, end = 7)) %>%
  sub("_0", "_", .)

names(full_sf) <- c(new_names, "geometry")

# Round values and reconstruct sf object
glorys_processed <- full_sf %>%
  sf::st_drop_geometry() %>%
  round(digits = 6) %>%
  sf::st_as_sf(geometry = sf::st_geometry(full_sf))

time_columns <- setdiff(names(glorys_processed), "geometry")

glorys_long <- glorys_processed %>%
  tidyr::pivot_longer(
    cols = all_of(time_columns),
    names_to = "time_descriptor",
    values_to = "value"
  ) %>%
  # Clean up time_descriptor to be more readable
  mutate(
    time_descriptor = gsub("X", "", time_descriptor),  # Remove X prefix
    time_descriptor = gsub("_", "-", time_descriptor)  # Convert underscores to dashes
  ) %>%
  # Remove rows with NA values
  filter(!is.na(value))

# Add metadata as attributes (for compatibility with as_ea_st)
attr(glorys_long, "units") <- "Temperature (\u00B0C)"
attr(glorys_long, "region") <- "Northwest Atlantic"
attr(glorys_long, "source") <- "Copernicus Marine Environment Monitoring Service (CMEMS), Global Ocean Physics Reanalysis"
attr(glorys_long, "time_descriptor") <- "Monthly data for 2018"
# Convert to ea_st object

glorys_bottom_temperature <- as_ea_st(
  spatial_obj = glorys_long,
  value_col = 'value',
  data_type = "Bottom Temperature",
  region = "Northwest Atlantic",
  time_descriptor = 'time_descriptor',
  units = "Temperature (degC)",
  source_citation = "Copernicus Marine Environment Monitoring Service (CMEMS), Global Ocean Physics Reanalysis",
  temporal_coverage = "2018 monthly data",
  variable_name = "bottomT",
  dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m"
)

# Clean up temporary file
file.remove(nc_file)

# Save the data
usethis::use_data(glorys_bottom_temperature, overwrite = TRUE)

# Optional: Print summary
cat("GLORYS bottom temperature data processed successfully\n")
cat("Data class:", class(glorys_bottom_temperature), "\n")
cat("Data dimensions:", dim(glorys_bottom_temperature$data), "\n")
cat("Data size:", format(object.size(glorys_bottom_temperature), units = "Kb"), "\n")