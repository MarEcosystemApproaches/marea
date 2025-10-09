# A variety of ecological indicators calculated from marindicators
# Run code line-by-line 

library(tidyverse)
library(marea)
library(here)


# From Jamie C. Tam on August 1, 2025
# updated by Emily O'Grady September 8 2025
# updated by Jamie C. Tam on Sept 15, 2025

# These are outputs are for two spatial levels of indicators calculated from RV Summer Ecosystem Survey data and commercial data (fisheries landings) using the Rpackage marindicators. 

# Citation:
# @article{Bundy2017,
# author = {Alida Bundy and Catalina Gomez and Adam M Cook},
# city = {Dartmouth, Nova Scotia},
# institution = {Fisheries and Oceans Canada, Bedford Institute of Oceanography},
# journal = {Canadian Technical Report of Fisheries and Aquatic Science},
# pages = {1-226},
# title = {Guidance framework for the selection and evaluation of ecological indicators},
# volume = {3232},
# year = {2017}
# }

# This is the raw data

data_dir <- 'R:/Science/BIODataSvc/SRC/marea'
eco_indicators_nafo<- file.path(data_dir, "eco_indicators_nafo.csv")
eco_indicators_nafo <- read_csv(eco_indicators_nafo)
eco_indicators_esswss<-file.path(data_dir, "eco_indicators_esswss.csv")
eco_indicators_esswss <- read_csv(eco_indicators_esswss)


#join the 2 data frames
join_indicators<-bind_rows(eco_indicators_nafo, eco_indicators_esswss) 

# filter years -2021, and only to 2022 when the RV survey changed to the Jacques Cartier, still no conversion factors for all the species required for this analysis. 

eco_indicators<-join_indicators |>
  rename(year=YEAR, region=ID) |> 
  select(!ends_with("_s")) # remove standardized data


#create "ea_data" object with multiple value columns
val_col_list <- names(eco_indicators)[!(names(eco_indicators) %in% c("year", "region"))]

# Create object with additional metadata
eco_indicators<- as_ea_data(
  x = eco_indicators,
  value_col = val_col_list, 
  data_type = "ecological",
  region = "Maritimes",
  location_descriptor = "NAFO",
  units = "tonnes",
  source_citation = "Bundy et al. 2017",
  nb = "years 2018 and 2021 have incomplete data due to poor survey coverage or ship change",
)


save(eco_indicators, file = here("data-raw", "eco-indicators", "eco_indicators.rda"))
usethis::use_data(eco_indicators, overwrite = TRUE)


