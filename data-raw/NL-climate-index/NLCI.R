# Newfoundland and Labrador Climate index
# Run code line-by-line and check plots.
#  See ?NLCI for details.

library(tidyverse)
library(marea)
# From Fred Cyr and Peter Galbraith.
# data-raw file created by Jamie C. Tam

# The NL Climate Index is included in the AZMP Atlantic zone report. Data is stored as .csv files here: https://doi.org/10.20383/101.0301

# Citation:
# Cyr, F. and Galbraith, P. S.: A climate index for the Newfoundland and Labrador shelf, Earth Syst. Sci. Data, 13, 1807–1828, https://doi.org/10.5194/essd-13-1807-2021, 2021.

# This the final analysed NL climate index, all fields are also available at the data website.

# This data raw process begins with downloading the latest .csv file into a local download folder.

fp <- "C:/Users/tamj/Downloads/" #adjust filepath as needed
NLCI_data_raw <-
  readr::read_csv(paste0(fp, "NL_climate_index.csv")) #df with 2 variables


NLCI <- NLCI_data_raw %>%
  rename(year = "Year",
        nlci="Climate index"
         )

nlci<- ea_data(
  data = NLCI,
  value_col = "nlci",
  data_type = 'NL Climate Index',
  units = "",
  species = "subindices: Winter North Atlantic Oscillation, air temperature, sea ice, icebergs, sea surface temperature, vertically-averaged temperature and salinity at Station~27, cold intermediate layer (CIL) core temperature at Station~27, CIL area on 3 hydrographic sections and bottom temperature on the NL shelf",
  region = "Newfoundland and Labrador Shelf",
  location_descriptor = 'Newfoundland and Labrador Shelf',
  source_citation = "Cyr, F. and Galbraith, P. S. 2021. A climate index for the Newfoundland and Labrador shelf, Earth Syst. Sci. Data, 13, 1807–1828, https://doi.org/10.5194/essd-13-1807-2021",
  comment = paste0("Data and associated sources are from https://doi.org/10.20383/101.0301")
)

usethis::use_data(nlci,
                  overwrite = TRUE)
