# Grey Seal estimates of total abundance
# Run code line-by-line and check plots.
#  See ?grey-seals for details.

library(dplyr)
library(ggplot2)
library(lubridate)

assess_yr <- 2021       # Year of the seal assessment; update each year

# From Nell den Heyer, 26th March 2025.

# The excel file here includes Sable and Scotian Shelf (Sable plus Hay and SWNS) estimates.  For simplicity can report Scotian Shelf and cite the assessment which reports the Scotian Shelf.  The trends are the same because the model largely informed by the Sable and all its estimates of demographic rates.

# Citation:
# Hammill, M.O., S.P. Rossi, A. Mosnier, C.E. den Heyer,  W.D. Bowen and G.B. Stenson. 2023.  Grey Seal Abundance and Harvest Advice in Canadian Waters. DFO Can. Sci. Advis. Sec. Res. Doc. 2023/053. vi + 40 p.

# This is the raw data
grey_seals_data_raw <-
  readr::read_csv("2021GreySealModel_SableEstimate-ModelResultsScotianShelf.csv",
                  skip = 1)

grey_seals_data_raw

summary(grey_seals_data_raw)

grey_seals_new <- grey_seals_data_raw %>%
  select("year",
         "lower95...11",
         "median...12",
         "upper95...13") %>%
  rename(low = "lower95...11",
         median = "median...12",
         high = "upper95...13")

class(grey_seals_new) <- c("pacea_biomass",
                              class(grey_seals_new))
attr(grey_seals_new, "axis_name") <-
  "Estimated abundance (number of seals, 1000s)"

grey_seals <- grey_seals_new

assign(paste0("grey_seals_", assess_yr),
       grey_seals_new)

usethis::use_data(grey_seals,
                  overwrite = TRUE)

pacea::create_data(paste0("grey_seals_", assess_yr),
                   get(paste0("grey_seals_", assess_yr)))
