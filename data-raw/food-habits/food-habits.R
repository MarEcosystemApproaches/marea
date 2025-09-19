# Food habits database
# Run code line-by-line 

library(tidyverse)
library(marea)
library(here)


# From Manon Cassista-Da-Ros from January 2025
# updated by Jamie C. Tam September 10, 2025



# This is the raw data extracted by Manon

data_dir <- 'R:/Science/BIODataSvc/SRC/marea'
food_habits<- file.path(data_dir, "FH.Eco.Surv.Data.Jan.2025.csv")
food_habits <- read_csv(food_habits)

# Citation:
# @article{CookandBundy2010,
# author = {Adam M. Cook, Alida Bundy},
# city = {Dartmouth, Nova Scotia},
# institution = {Fisheries and Oceans Canada, Bedford Institute of Oceanography},
# journal = {Canadian Technical Report of Fisheries and Aquatic Science},
# pages = {1-140},
# title = {The Food Habits Database: an updat,determination of sampling adequacy and estimation of diet for key species},
# volume = {2884},
# year = {2010}
# }

# rename year column for compatability with ea_data()
food_habits<-food_habits |>
  rename(year=YEAR, region=NAFO_ZONE) 


#create "ea_data" object with multiple value columns
val_col_list <- names(food_habits)[!(names(food_habits) %in% c("year", "region"))]

# Create object with additional metadata
food_habits<- ea_data(
  data = food_habits,
  value_col = val_col_list, 
  data_type = "biological",
  region = "Maritimes",
  location_descriptor = "NAFO 4",
  units = "weights",
  source_citation = "Cook and Bundy 2010",
)


save(food_habits, file = here("data-raw", "food-habits", "food_habits.rda"))
usethis::use_data(food_habits, overwrite = TRUE)
