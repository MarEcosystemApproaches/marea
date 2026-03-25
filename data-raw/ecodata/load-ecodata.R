# GSNW Data from ecodata Package
# Transforms Gulf Stream North Wall (GSNW) index data into marea ea_object format
# and saves to the marea package data directory

# ---- Setup ----
devtools::load_all()

library(dplyr)
library(tibble)
library(marea)
library(pak)

# Load ecodata package quietly
# A fresh install should always grab the latest available data
suppressPackageStartupMessages({
  pak::pak("noaa-edab/ecodata")
  library(ecodata)
})

# ---- Helper Function ----
make_ea_index <- function(
    df,
    value_col,
    data_type,
    region,
    location,
    units,
    source,
    species = NA_character_,
    ...
) {
  ea_data(
    data                = df,
    value_col           = value_col,
    data_type           = data_type,
    region              = region,
    location_descriptor = location,
    units               = units,
    species             = species,
    source_citation     = source,
    ...
  )
}

# ---- Inspect GSNW Data ----
# ecodata::gsnw contains Gulf Stream North Wall index
# Typical columns: Time, Var, Value, EPU, Units
glimpse(ecodata::gsi)

# ---- Transform GSNW Data ----
gsnw_new <- ecodata::gsi |>
  as_tibble() |>
  filter(Var == "gulf stream index") |>          # retain the index variable only
  transmute(
    year  = as.integer(Time),                    # coerce year to integer
    value = as.numeric(Value)                    # ensure numeric value
  ) |>
  filter(!is.na(value))                          # drop missing observations

# ---- Create ea_data Object ----
gsnw <- make_ea_index(
  df        = gsnw_new,
  value_col = "value",
  data_type = "Gulf Stream North Wall Index",
  region    = "Northwest Atlantic",
  location  = "Gulf Stream North Wall position index",
  units     = "degrees latitude anomaly",
  source    = paste0(
    "NOAA EDAB ecodata R package (https://noaa-edab.github.io/ecodata/); ",
    "Joyce, T.M. and Zhang, R. (2010). On the path of the Gulf Stream and the ",
    "Atlantic meridional overturning circulation. Journal of Climate, 23(11), ",
    "3146-3154. https://doi.org/10.1175/2010JCLI3310.1"
  ),
  details   = paste0(
    "The Gulf Stream North Wall (GSNW) index measures the latitudinal ",
    "position of the Gulf Stream's northern wall, calculated as an anomaly ",
    "from the long-term mean position. A positive index indicates a more ",
    "northerly Gulf Stream position. The index has been linked to changes in ",
    "shelf water temperature, stratification, and ecosystem productivity on ",
    "the Northeast US Continental Shelf."
  )
)

# ---- Save to marea Package ----
usethis::use_data(gsnw, overwrite = TRUE)
