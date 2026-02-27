# Food habits database
# Run code line-by-line.

library(dplyr)
library(here)

# From Manon Cassista-Da-Ros from January 2025
# updated by Jamie C. Tam September 10, 2025
# updated by inSileco team February 2026

# Citation:
# @article{CookandBundy2010,
# author = {Adam M. Cook, Alida Bundy},
# city = {Dartmouth, Nova Scotia},
# institution = {Fisheries and Oceans Canada, Bedford Institute of Oceanography},
# journal = {Canadian Technical Report of Fisheries and Aquatic Science},
# pages = {1-140},
# title = {The Food Habits Database: an update, determination of sampling adequacy and estimation of diet for key species},
# volume = {2884},
# year = {2010}
# }

# Exported food-habits databases created by this script:
# - food_habits_full: primary cleaned stomach-record table.
# - food_habits_stomach: detailed standardized stomach/prey table with joined species names.
# - food_habits_species: species code dictionary used for predator/prey lookups.
# - food_habits_mean_diet_stratified: stratified mean diet estimates (area + length strata).
# - food_habits_dominant_prey_timeseries: dominant prey species over time.
# - food_habits_prey_predation: predator contributions to focal prey consumption.

# ---- Source utility functions ----
source(here("data-raw", "food-habits", "food-habits-utils.R"))
source(here("data-raw", "food-habits", "food-habits-estimate-mean-diet.R"))
source(here("data-raw", "food-habits", "food-habits-estimate-dominant-prey.R"))
source(here("data-raw", "food-habits", "food-habits-estimate-predator-contribution.R"))
source(here("data-raw", "food-habits", "food-habits-checks.R"))

# ---- User-configurable settings ----

# Set source_mode to "mar_datawrangling" when client credentials/code are available.
source_mode <- "local_rdata"

# Priority species discussed with managers/assessment leads.
priority_predators <- c(
  "ATLANTIC COD", "HADDOCK", "POLLOCK", "SILVER HAKE", "REDFISH", "AMERICAN PLAICE"
)
priority_prey <- c(
  "ATLANTIC HERRING", "PANDALUS BOREALIS", "GREEN SEA URCHIN", "SEA URCHIN"
)

# Grouping controls for summary products.
# Add/remove fields based on desired output granularity.
mean_diet_group_vars <- c("year", "strat", "pred_code")
dominant_prey_group_vars <- c("year", "pred_code")
prey_predation_group_vars <- c("year", "nafo_zone", "prey_code")

# Set TRUE to automatically include name labels for code groupings
# (e.g., pred_common for pred_code).
include_label_cols <- TRUE

# Dominance/predation reporting thresholds.
dominant_prey_top_n <- 5
dominant_prey_min_prop <- NULL
dominant_prey_min_occurrence <- NULL
prey_predation_top_n_predators <- NULL
prey_predation_min_contribution <- NULL

# Plot export mode:
# - "per_species": one figure per species of interest (default).
# - "aggregated": one figure per output aggregating all selected species.
plot_export_mode <- "per_species"
run_contract_checks <- TRUE

# ---- Build data products ----

inputs <- load_food_habits_inputs(source_mode = source_mode)
std <- standardize_food_habits(
  stomach_raw = inputs$STOMACH_DATA_VW,
  species_raw = inputs$GSSPECIES
)

food_habits_stomach <- std$stomach
food_habits_species <- std$species_lookup

food_habits_qc(food_habits_stomach, food_habits_species)

priority_predator_codes <- resolve_priority_codes(food_habits_species, priority_predators)
priority_prey_codes <- resolve_priority_codes(food_habits_species, priority_prey)

food_habits_priority <- filter_food_habits(
  food_habits_stomach,
  predator_codes = priority_predator_codes
)

# Output table 1 (stratified mean diet):
# - Uses only priority predator species selected above.
# - Computes mean diet by prey with default grouping in `mean_diet_group_vars`
#   (currently year + survey stratum + predator code).
# - Length structure is handled inside `estimate_mean_diet()` to reflect
#   length-stratified stomach sampling.
food_habits_mean_diet_stratified <- estimate_mean_diet(
  food_habits_stomach = food_habits_priority,
  group_vars = mean_diet_group_vars,
  include_label_cols = include_label_cols
)

# Output table 2 (dominant prey):
# - Starts from the same priority-predator filtered data.
# - Aggregates using `dominant_prey_group_vars` (currently year + predator code)
#   to create a time-series-style table.
# - Keeps dominant prey using configurable thresholds (`dominant_prey_top_n`,
#   `dominant_prey_min_prop`, `dominant_prey_min_occurrence`).
food_habits_dominant_prey_timeseries <- estimate_dominant_prey(
  food_habits_stomach = food_habits_priority,
  group_vars = dominant_prey_group_vars,
  top_n = dominant_prey_top_n,
  min_diet_prop = dominant_prey_min_prop,
  min_occurrence_prop = dominant_prey_min_occurrence,
  include_label_cols = include_label_cols
)

# Output table 3 (predator contribution on focal prey):
# - Filters to priority prey species before estimation.
# - Aggregates predator contribution with `prey_predation_group_vars`
#   (currently year + NAFO zone + prey code), which gives spatially aggregated
#   prey-centric predation summaries.
# - Optional filters keep only dominant predator contributors by rank or minimum
#   contribution proportion.
food_habits_prey_predation <- estimate_predator_contribution(
  food_habits_stomach = filter_food_habits(food_habits_stomach, prey_codes = priority_prey_codes),
  group_vars = prey_predation_group_vars,
  include_label_cols = include_label_cols,
  top_n_predators = prey_predation_top_n_predators,
  min_predator_contribution = prey_predation_min_contribution
)

if (isTRUE(run_contract_checks)) {
  run_food_habits_contract_checks(
    food_habits_stomach = food_habits_stomach,
    food_habits_species = food_habits_species,
    food_habits_mean_diet_stratified = food_habits_mean_diet_stratified,
    food_habits_dominant_prey_timeseries = food_habits_dominant_prey_timeseries,
    food_habits_prey_predation = food_habits_prey_predation,
    priority_predator_codes = priority_predator_codes,
    priority_prey_codes = priority_prey_codes,
    mean_diet_group_vars = mean_diet_group_vars,
    dominant_prey_group_vars = dominant_prey_group_vars,
    prey_predation_group_vars = prey_predation_group_vars
  )
}

# ---- Export summary figures ----
export_food_habits_plots(
  food_habits_mean_diet_stratified = food_habits_mean_diet_stratified,
  food_habits_dominant_prey_timeseries = food_habits_dominant_prey_timeseries,
  food_habits_prey_predation = food_habits_prey_predation,
  food_habits_species = food_habits_species,
  priority_predator_codes = priority_predator_codes,
  priority_prey_codes = priority_prey_codes,
  plot_export_mode = plot_export_mode,
  out_dir = here("data-raw", "food-habits")
)

food_habits_full <- food_habits_stomach
attr(food_habits_full, "source_citation") <- "Cook and Bundy 2010; Mar.datawrangling extraction workflow"
attr(food_habits_full, "region") <- "Maritimes"

save(food_habits_full, file = here("data-raw", "food-habits", "food_habits_full.rda"))
save(food_habits_stomach, file = here("data-raw", "food-habits", "food_habits_stomach.rda"))
save(food_habits_species, file = here("data-raw", "food-habits", "food_habits_species.rda"))
save(food_habits_mean_diet_stratified, file = here("data-raw", "food-habits", "food_habits_mean_diet_stratified.rda"))
save(food_habits_dominant_prey_timeseries, file = here("data-raw", "food-habits", "food_habits_dominant_prey_timeseries.rda"))
save(food_habits_prey_predation, file = here("data-raw", "food-habits", "food_habits_prey_predation.rda"))

usethis::use_data(
  food_habits_full,
  food_habits_stomach,
  food_habits_species,
  food_habits_mean_diet_stratified,
  food_habits_dominant_prey_timeseries,
  food_habits_prey_predation,
  overwrite = TRUE
)
