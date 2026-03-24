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
  year   = azmpdata::Derived_Annual_Carbonate$year,
  section = azmpdata::Derived_Annual_Carbonate$section_name,
  depth = azmpdata::Derived_Annual_Carbonate$depth_m,
  TA_mean_umolkg   = azmpdata::Derived_Annual_Carbonate$mean_TA_umolkg,
  DIC_mean_umolkg  = azmpdata::Derived_Annual_Carbonate$mean_DIC_umolkg,
  pH_mean         = azmpdata::Derived_Annual_Carbonate$mean_pH_total,
  pco2_mean_uatm   = azmpdata::Derived_Annual_Carbonate$mean_pCO2_uatm,
  substrate_inhibitor_ratio = azmpdata::Derived_Annual_Carbonate$mean_substrate_inhibitor_ratio,
  omega_aragonite_mean = azmpdata::Derived_Annual_Carbonate$mean_omega_aragonite,
  omega_calcite_mean = azmpdata::Derived_Annual_Carbonate$mean_omega_calcite,
  carbonate_system_vulnerability_index = azmpdata::Derived_Annual_Carbonate$mean_carbonate_system_vulnerability_index
) 

azmp_carbonate <- make_ea_index(
  df = carbonate_new,
  value_col = c("TA_mean_umolkg", 
                "DIC_mean_umolkg", 
                "pH_mean", 
                "pco2_mean_uatm",
                "substrate_inhibitor_ratio",
                "omega_aragonite_mean",
                "omega_calcite_mean",
                "carbonate_system_vulnerability_index"),
  data_type = "Synthesized Carbonate Indices",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "Maritimes region",
  units = "TA = umol/kg, DIC = umol/kg, pH = total scale, pCO2 = uatm, substrate-inhibitor ratio = unitless, omega aragonite = unitless, omega calcite = unitless, carbonate system vulnerability index = unitless",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  principal_investigator = "Kumiko Azetsu-Scott (Kumiko.Azetsu-Scott@dfo-mpo.gc.ca)",
  citation = "Lizotte, M., Blais, M., Chassé, J., Galbraith, P. S., Hébert, A.-J., Starr, M. 2026. Acidification and CO2-Driven Conditions in the Estuary and Gulf of St. Lawrence During 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 410 : vi + 71 p."
)
usethis::use_data(azmp_carbonate, overwrite = TRUE)
