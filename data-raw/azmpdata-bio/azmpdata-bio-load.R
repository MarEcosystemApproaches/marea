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

# ---- AZMP Phytoplankton ----
azmp_phytoplankton_new <- tibble(
  year   = azmpdata::Phytoplankton_Annual_Stations$year,
  region = azmpdata::Phytoplankton_Annual_Stations$station,
  diatoms   = azmpdata::Phytoplankton_Annual_Stations$diatoms_log10,
  dinoflaggelates = azmpdata::Phytoplankton_Annual_Stations$dinoflagellates_log10,
  flagellates = azmpdata::Phytoplankton_Annual_Stations$flagellates_log10,
  cilliates = azmpdata::Phytoplankton_Annual_Stations$ciliates_log10
) 

azmp_phytoplankton <- make_ea_index(
  df = azmp_phytoplankton_new,
  value_col = "diatoms",
  data_type = "phytoplankton",
  region = "Halifax2 and Prince5 stations",
  location = "Scotian Shelf and Bay of Funday",
  units = "log10",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Casault, B., Johnson, C.L., Devred, E., Clay, S., and Beazley, L. 2025. Chemical and Biological Oceanographic Conditions on the Scotian Shelf and in the Eastern Gulf of Maine during 2024. Can. Tech. Rep. Fish. Aquat. Sci. 3744: vi + 58 p. https://doi.org/10.60825/p6ad-1k52"
)

usethis::use_data(azmp_phytoplankton, overwrite = TRUE)

# ---- AZMP zooplankton ----
azmp_zooplankton_new <- tibble(
  year   = azmpdata::Zooplankton_Annual_Sections$year,
  region = azmpdata::Zooplankton_Annual_Sections$section,
  calanus_fin   = azmpdata::Zooplankton_Annual_Sections$Calanus_finmarchicus_log10,
  pseudocalanus = azmpdata::Zooplankton_Annual_Sections$Pseudocalanus_log10,
  copepods = azmpdata::Zooplankton_Annual_Sections$copepods_log10,
  non_copepods = azmpdata::Zooplankton_Annual_Sections$non_copepods_log10,
  arctic_calanus_spp = azmpdata::Zooplankton_Annual_Sections$Arctic_Calanus_species_log10,
  warm_offshore_copepods = azmpdata::Zooplankton_Annual_Sections$warm_offshore_copepods_log10,
  warm_shelf_copepods = azmpdata::Zooplankton_Annual_Sections$warm_shelf_copepods_log10,
  meso_zooplankton = azmpdata::Zooplankton_Annual_Sections$zooplankton_meso_dry_weight
) 

azmp_zooplankton <- make_ea_index(
  df = azmp_zooplankton_new,
  value_col = "calanus_fin",
  data_type = "zooplankton",
  region = "Cabot Strait Line, Louisbourg Line, Halifax Line, Browns Bank Line ",
  location = "Scotian Shelf",
  units = "log10",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Casault, B., Johnson, C.L., Devred, E., Clay, S., and Beazley, L. 2025. Chemical and Biological Oceanographic Conditions on the Scotian Shelf and in the Eastern Gulf of Maine during 2024. Can. Tech. Rep. Fish. Aquat. Sci. 3744: vi + 58 p. https://doi.org/10.60825/p6ad-1k52"
)

usethis::use_data(azmp_zooplankton, overwrite = TRUE)

# ---- AZMP chemical ----

azmp_chemical_new <- tibble(
  year   = azmpdata::Derived_Annual_Sections$year,
  region = azmpdata::Derived_Annual_Sections$section,
  chlorophyll_0_100   = azmpdata::Derived_Annual_Sections$integrated_chlorophyll_0_100,
  integrated_nitrate_0_50 = azmpdata::Derived_Annual_Sections$integrated_nitrate_0_50,
  integrated_nitrate_50_150 = azmpdata::Derived_Annual_Sections$integrated_nitrate_50_150,
  integrated_phosphate_0_50 = azmpdata::Derived_Annual_Sections$integrated_phosphate_0_50,
  integrated_phosphate_50_150 = azmpdata::Derived_Annual_Sections$integrated_phosphate_50_150,
  integrated_silicate_0_50   = azmpdata::Derived_Annual_Sections$integrated_silicate_0_50,
  integrated_silicate_50_150  = azmpdata::Derived_Annual_Sections$integrated_silicate_50_150
) 

azmp_chemical <- make_ea_index(
  df = azmp_chemical_new,
  value_col = "chlorophyll_0_100",
  data_type = "chemical",
  region = "Cabot strait Line, Louisbourg Line, Halifax Line, Browns Bank Line ",
  location = "Scotian Shelf",
  units = "chemical",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Casault, B., Johnson, C.L., Devred, E., Clay, S., and Beazley, L. 2025. Chemical and Biological Oceanographic Conditions on the Scotian Shelf and in the Eastern Gulf of Maine during 2024. Can. Tech. Rep. Fish. Aquat. Sci. 3744: vi + 58 p. https://doi.org/10.60825/p6ad-1k52"
)
usethis::use_data(azmp_chemical, overwrite = TRUE)

# ---- AZMP remote sensing ----

azmp_remotesensing_new <- tibble(
  year   = azmpdata::RemoteSensing_Annual_Broadscale$year,
  region = azmpdata::RemoteSensing_Annual_Broadscale$area,
  surface_chlorophyll_log10  = azmpdata::RemoteSensing_Annual_Broadscale$surface_chlorophyll_log10,
  bloom_start = azmpdata::RemoteSensing_Annual_Broadscale$bloom_start,
  bloom_duration = azmpdata::RemoteSensing_Annual_Broadscale$bloom_duration,
  bloom_amplitude = azmpdata::RemoteSensing_Annual_Broadscale$bloom_amplitude,
  bloom_magnitude= azmpdata::RemoteSensing_Annual_Broadscale$bloom_magnitude
) 

azmp_remotesensing <- make_ea_index(
  df = azmp_remotesensing_new,
  value_col = "surface_chlorophyll_log10",
  data_type = "bloom statistics",
  region = "Cabot Strait, Eastern Scotian Shelf, Central Scotian Shelf, Western Scotian Shelf, Georges Bank, Lower Shelf ",
  location = "Scotian Shelf",
  units = "log10",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Casault, B., Johnson, C.L., Devred, E., Clay, S., and Beazley, L. 2025. Chemical and Biological Oceanographic Conditions on the Scotian Shelf and in the Eastern Gulf of Maine during 2024. Can. Tech. Rep. Fish. Aquat. Sci. 3744: vi + 58 p. https://doi.org/10.60825/p6ad-1k52"
)

usethis::use_data(azmp_remotesensing, overwrite = TRUE)