# A variety of ecological indicators calculated from marindicators
# Run code line-by-line 

library(dplyr)
library(marea)
library(here)


# From Jamie C. Tam on August 1, 2025

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

dat.dir<-here::here("data-raw/eco-indicators")
eco_indicators_nafo<- "eco_indicators_nafo.Rdata"
load(file.path(dat.dir, eco_indicators_nafo))
eco_indicators_esswss<-"eco_indicators_esswss.Rdata"
load(file.path(dat.dir, eco_indicators_esswss))

# have to rename the esswss file, will change this in future iterations when saving from marinidicators.     
eco_indicators_esswss<-allIndicators


#join the 2 dataframes
join_indicators<-bind_rows(eco_indicators_nafo, eco_indicators_esswss) 

# filter years <2021 when the RV survey changed to the Jacques Cartier, still no conversion factors for all the species required for this analysis. 

eco_indicators<-join_indicators |>
  filter(YEAR<2021) |> 
  rename(year=YEAR, region=ID)


#create "ea_data" object

# Create object with additional metadata
eco_indicators<- ea_data(
  data = eco_indicators,
  value_col = "SpeciesRichness_ALL", 
  data_type = "diversity",
  region = "Maritimes",
  location_descriptor = "NAFO",
  units = "tonnes",
  source_citation = "Bundy et al. 2017",
)

#TODO change "value" column, rename to "SpeciesRichness" 
#this sort of worked, but didn't save it to the ea_data object.

eco_indicators@data |> rename(SpeciesRichness_ALL=value)

save(eco_indicators, file = here("data-raw", "eco-indicators", "eco_indicators.rda"))
usethis::use_data(eco_indicators, overwrite = TRUE)


