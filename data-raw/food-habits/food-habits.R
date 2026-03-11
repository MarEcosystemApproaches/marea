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

# ------------------- Source utility functions -------------------
source(here("data-raw", "food-habits", "food-habits-utils.R"))
source(here("data-raw", "food-habits", "food-habits-estimate-mean-diet.R"))
source(here("data-raw", "food-habits", "food-habits-estimate-dominant-prey.R"))
source(here("data-raw", "food-habits", "food-habits-estimate-predator-contribution.R"))


# ------------------- User-configurable settings -------------------

# Set source_mode to "mar_datawrangling" when client credentials/code are available.
# TODO: Implementation should be done in function `load_food_habits_inputs()` in food-habits-utils.R
source_mode <- "local_rdata"

# Priority species discussed with managers/assessment leads.
# Use species codes as stable identifiers; comments are for readability.
priority_predator_codes <- c(
  10, # COD(ATLANTIC)
  11, # HADDOCK
  16, # POLLOCK
  14, # SILVER HAKE
  20, # REDFISH
  40 # AMERICAN PLAICE
)
priority_prey_codes <- c(
  60, # HERRING(ATLANTIC)
  2211, # PANDALUS BOREALIS
  6411, # SEA URCHIN (GREEN)
  6400 # SEA URCHINS
)

# Optional predator/prey grouping (code-based).
# Example:
# prey_group_definitions <- list(
#   pandalus_spp = list(
#     members = c(2210, 2211),  # PANDALUS SP. + PANDALUS BOREALIS
#     new_code = 2211,          # keep canonical code
#     new_label = "PANDALUS SPP"
#   )
# )
apply_predator_grouping_flag <- FALSE
predator_group_definitions <- list()
apply_prey_grouping_flag <- FALSE
prey_group_definitions <- list()

# Grouping controls for summary products.
# Add/remove fields based on desired output granularity.
mean_diet_group_vars <- c("year", "strat", "pred_code")
dominant_prey_group_vars <- c("year", "pred_code")
prey_predation_group_vars <- c("year", "nafo_zone", "prey_code")

# Length-bin settings used by methods that rely on `estimate_mean_diet()`.
# Clients can modify these directly to change length stratification behavior.
mean_diet_length_breaks <- seq(0, 150, by = 5)
dominant_prey_length_breaks <- mean_diet_length_breaks

# Set TRUE to automatically include name labels for code groupings
# (e.g., pred_common for pred_code).
include_label_cols <- TRUE

# Exclude digestion/artefact/parasite prey codes from diet products.
# Set FALSE to retain all prey codes.
remove_excluded_codes <- TRUE
excluded_prey_codes <- food_habits_default_exclusion_prey_codes()

# Denominator mode for mean-diet metrics:
# - "usable_predators": only predators with at least one usable prey row after
#   prey/weight/exclusion filters (historical behavior).
# - "all_sampled_predators": all sampled predators with non-missing
#   pred_seq/strat/length-bin, even if no usable prey row remains.
mean_diet_denominator_mode <- "usable_predators"

# Dominance/predation reporting thresholds.
dominant_prey_top_n <- 5
dominant_prey_min_prop <- NULL
dominant_prey_min_occurrence <- NULL
prey_predation_top_n_predators <- NULL
prey_predation_min_contribution <- NULL

# Example outputs and figures flag
# Set to TRUE to generate examples and figures scripted at the end of this file
run_examples <- TRUE

# ------------------- Build data products -------------------

inputs <- load_food_habits_inputs(source_mode = source_mode)
std <- standardize_food_habits(
  stomach_raw = inputs$STOMACH_DATA_VW,
  species_raw = inputs$GSSPECIES
)

food_habits_stomach <- std$stomach
food_habits_species <- std$species_lookup

# Build a single processed analysis table:
# - filters to priority predators
# - optionally applies prey/predator grouping
food_habits_processed <- filter_food_habits(
  food_habits_stomach,
  predator_codes = priority_predator_codes
)
if (isTRUE(apply_prey_grouping_flag)) {
  food_habits_processed <- apply_code_grouping(
    data = food_habits_processed,
    group_definitions = prey_group_definitions,
    code_var = "prey_code",
    label_var = "prey_common"
  )
}
if (isTRUE(apply_predator_grouping_flag)) {
  food_habits_processed <- apply_code_grouping(
    data = food_habits_processed,
    group_definitions = predator_group_definitions,
    code_var = "pred_code",
    label_var = "pred_common"
  )
}
priority_prey_codes_processed <- map_codes_to_grouped_codes(
  codes = priority_prey_codes,
  group_definitions = if (isTRUE(apply_prey_grouping_flag)) prey_group_definitions else list()
)
priority_predator_codes_processed <- map_codes_to_grouped_codes(
  codes = priority_predator_codes,
  group_definitions = if (isTRUE(apply_predator_grouping_flag)) predator_group_definitions else list()
)

# Output table 1 (stratified mean diet):
# - Uses only priority predator species selected above.
# - Computes mean diet by prey with default grouping in `mean_diet_group_vars`
#   (currently year + survey stratum + predator code).
# - Length structure is handled inside `estimate_mean_diet()` to reflect
#   length-stratified stomach sampling.
# - Denominator scope is controlled by `mean_diet_denominator_mode`.
food_habits_mean_diet_stratified <- estimate_mean_diet(
  food_habits_stomach = food_habits_processed,
  group_vars = mean_diet_group_vars,
  length_breaks = mean_diet_length_breaks,
  remove_excluded_codes = remove_excluded_codes,
  excluded_prey_codes = excluded_prey_codes,
  denominator_mode = mean_diet_denominator_mode,
  include_label_cols = include_label_cols
)


# Output table 2 (dominant prey):
# - Starts from the same priority-predator filtered data.
# - Aggregates using `dominant_prey_group_vars` (currently year + predator code)
#   to create a time-series-style table.
# - Keeps dominant prey using configurable thresholds (`dominant_prey_top_n`,
#   `dominant_prey_min_prop`, `dominant_prey_min_occurrence`).
# - Uses the same mean-diet denominator choice via `mean_diet_denominator_mode`.
food_habits_dominant_prey_timeseries <- estimate_dominant_prey(
  food_habits_stomach = food_habits_processed,
  group_vars = dominant_prey_group_vars,
  length_breaks = dominant_prey_length_breaks,
  remove_excluded_codes = remove_excluded_codes,
  excluded_prey_codes = excluded_prey_codes,
  denominator_mode = mean_diet_denominator_mode,
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
  food_habits_stomach = filter_food_habits(food_habits_processed, prey_codes = priority_prey_codes_processed),
  group_vars = prey_predation_group_vars,
  remove_excluded_codes = remove_excluded_codes,
  excluded_prey_codes = excluded_prey_codes,
  include_label_cols = include_label_cols,
  top_n_predators = prey_predation_top_n_predators,
  min_predator_contribution = prey_predation_min_contribution
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

# ------------------- Example outputs and summary figures -------------------

if (run_examples) {
  # Plot export mode:
  # - "per_species": one figure per species of interest (default).
  # - "aggregated": one figure per output aggregating all selected species.
  plot_export_mode <- "per_species"

  # Example output (unaggregated by strata): keep strata, collapse length bins.
  food_habits_mean_diet_by_strata_example <- estimate_mean_diet(
    food_habits_stomach = food_habits_processed,
    group_vars = c("pred_code"),
    length_breaks = mean_diet_length_breaks,
    remove_excluded_codes = remove_excluded_codes,
    excluded_prey_codes = excluded_prey_codes,
    denominator_mode = mean_diet_denominator_mode,
    retain_strata = TRUE,
    retain_length_bins = FALSE,
    include_label_cols = include_label_cols
  )

  # Example output (unaggregated by length): keep length bins, collapse strata.
  food_habits_mean_diet_by_length_example <- estimate_mean_diet(
    food_habits_stomach = food_habits_processed,
    group_vars = c("pred_code"),
    length_breaks = mean_diet_length_breaks,
    remove_excluded_codes = remove_excluded_codes,
    excluded_prey_codes = excluded_prey_codes,
    denominator_mode = mean_diet_denominator_mode,
    retain_strata = FALSE,
    retain_length_bins = TRUE,
    include_label_cols = include_label_cols
  )

  export_food_habits_plots(
    food_habits_mean_diet_stratified = food_habits_mean_diet_stratified,
    food_habits_dominant_prey_timeseries = food_habits_dominant_prey_timeseries,
    food_habits_prey_predation = food_habits_prey_predation,
    food_habits_species = food_habits_species,
    priority_predator_codes = priority_predator_codes_processed,
    priority_prey_codes = priority_prey_codes_processed,
    plot_export_mode = plot_export_mode,
    out_dir = here("data-raw", "food-habits")
  )

  # Additional example figures for unaggregated mean-diet outputs.
  export_food_habits_mean_diet_unaggregated_plots(
    food_habits_mean_diet_by_strata_example = food_habits_mean_diet_by_strata_example,
    food_habits_mean_diet_by_length_example = food_habits_mean_diet_by_length_example,
    out_dir = here("data-raw", "food-habits"),
    top_n = 12,
    length_bin_var = "length_bin"
  )
}
