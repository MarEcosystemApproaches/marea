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

fp <- 'R:/Science/BIODataSvc/SRC/marea/'
grey_seals_data_raw <-
  readr::read_csv(paste0(fp, "2021GreySealModel_SableEstimate-ModelResultsScotianShelf.csv"),
                  skip = 1)

# grey_seals_data_raw
# 
# summary(grey_seals_data_raw)

grey_seals_new <- grey_seals_data_raw %>%
  select("year",
         "lower95...11",
         "median...12",
         "upper95...13") %>%
  rename(lower = "lower95...11",
         median = "median...12",
         upper = "upper95...13")

grey_seals <- ea_data(
  data = grey_seals_new,
  value_col = "median",
  data_type = 'Grey Seal Abundance',
  year = assess_yr,
  units = "number of seals",
  species = "grey seal",
  region = "Scotian Shelf",
  location_descriptor = 'Sable Island',
  source_citation = "den Heyer, C. E., Mosnier, A., Stenson, G. B., Lidgard, D. C., Bowen, W. D., & Hammill, M. O. (2024). Grey seal pup production in Canada (DFO Can. Sci. Advis. Sec. Res. Doc. 2023/078). Fisheries and Oceans Canada, Canadian Science Advisory Secretariat.",
  comment = paste0("Estimates from a Bayesian state-space model fitted to aerial survey data and demographic rates from Sable Island. See Hammill et al. (2023) for details.")
)

usethis::use_data(grey_seals,
                  overwrite = TRUE)

# class(grey_seals_new) <- c("pacea_biomass",
#                               class(grey_seals_new))
# attr(grey_seals_new, "axis_name") <-
#   "Estimated abundance (number of seals, 1000s)"
# 
# grey_seals <- grey_seals_new
# 
# assign(paste0("grey_seals_", assess_yr),
#        grey_seals_new)
# 
# usethis::use_data(grey_seals,
#                   overwrite = TRUE)
# 
# pacea::create_data(paste0("grey_seals_", assess_yr),
#                    get(paste0("grey_seals_", assess_yr)))
