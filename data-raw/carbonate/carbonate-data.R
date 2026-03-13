# load synthesized carbonate data from azmpdata
# E O'Grady March 2026

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

carbonate_new <- tibble(
  year   = azmpdata::,
  region = azmpdata::,
  mean   = azmpdata::
) %>%
  filter(!is.na(mean))

azmp_carbonate <- make_ea_index(
  df = carbonate_new,
  value_col = c("mean", "others"),
  data_type = "Synthesized Carbonate Indices",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "Maritimes region",
  units = "list all variable units",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  principal_investigator = "Kumiko Azetsu-Scott (Kumiko.Azetsu-Scott@dfo-mpo.gc.ca)",
  citation = "Lizotte, M., Blais, M., Chassé, J., Galbraith, P. S., Hébert, A.-J., Starr, M. 2026. Acidification and CO2-Driven Conditions in the Estuary and Gulf of St. Lawrence During 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 410 : vi + 71 p."
)
usethis::use_data(azmp_bottom_temperature, overwrite = TRUE)
