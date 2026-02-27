# Food habits database
# Run code line-by-line.

library(dplyr)
library(stringr)
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

# Set source_mode to "mar_datawrangling" when client credentials/code are available.
source_mode <- "local_rdata"

# Exported food-habits databases created by this script:
# - food_habits: primary cleaned stomach-record table (backward-compatible object name).
# - food_habits_stomach: detailed standardized stomach/prey table with joined species names.
# - food_habits_species: species code dictionary used for predator/prey lookups.
# - food_habits_diet_composition: predator-focused prey composition summaries.
# - food_habits_prey_pressure: prey-focused predator contribution summaries.

# Resolve the location of an input .RData file from installed package data
# or local project files.
resolve_food_habits_path <- function(file_name) {
  pkg_path <- system.file("data-raw", "food-habits", file_name, package = "marea")
  if (nzchar(pkg_path) && file.exists(pkg_path)) {
    return(pkg_path)
  }

  local_path <- file.path("data-raw", "food-habits", file_name)
  if (file.exists(local_path)) {
    return(local_path)
  }

  stop("Could not locate file: ", file_name, call. = FALSE)
}

# Load raw food-habits inputs from local .RData files, with a placeholder
# branch for authenticated Mar.datawrangling retrieval.
load_food_habits_inputs <- function(source_mode = "local_rdata") {
  if (identical(source_mode, "mar_datawrangling")) {
    # TODO(client): Replace this placeholder with authenticated Mar.datawrangling code.
    # Example intent:
    # 1) Authenticate to internal data services.
    # 2) Pull groundfish stomach dataset (STOMACH_DATA_VW equivalent).
    # 3) Pull species dictionary (GSSPECIES equivalent).
    stop(
      "source_mode = 'mar_datawrangling' is a placeholder. ",
      "Client-provided authenticated retrieval code is still required.",
      call. = FALSE
    )
  }

  if (!identical(source_mode, "local_rdata")) {
    stop("source_mode must be either 'local_rdata' or 'mar_datawrangling'.", call. = FALSE)
  }

  stomach_fp <- resolve_food_habits_path("GROUNDFISH_STOMACH_DATA_VW.RData")
  species_fp <- resolve_food_habits_path("GROUNDFISH_GSSPECIES.RData")

  load(stomach_fp)
  load(species_fp)

  if (!exists("STOMACH_DATA_VW") || !exists("GSSPECIES")) {
    stop("Loaded files do not contain STOMACH_DATA_VW and/or GSSPECIES.", call. = FALSE)
  }

  list(
    STOMACH_DATA_VW = STOMACH_DATA_VW,
    GSSPECIES = GSSPECIES
  )
}

# Standardize raw stomach and species data into analysis-ready tables,
# including predator/prey species-name joins.
standardize_food_habits <- function(stomach_raw, species_raw) {
  species_lookup <- species_raw %>%
    mutate(
      species_code = as.integer(CODE),
      common_name = str_squish(as.character(COMM)),
      latin_name = str_squish(as.character(SPEC))
    ) %>%
    select(species_code, common_name, latin_name)

  pred_species <- species_lookup %>%
    rename(
      pred_code = species_code,
      pred_common = common_name,
      pred_latin = latin_name
    )

  prey_species <- species_lookup %>%
    rename(
      prey_code = species_code,
      prey_common = common_name,
      prey_latin = latin_name
    )

  food_habits_stomach <- stomach_raw %>%
    mutate(
      SDATE = as.Date(SDATE),
      year = as.integer(format(SDATE, "%Y")),
      month = as.integer(format(SDATE, "%m")),
      pred_code = as.integer(SPEC),
      prey_code = as.integer(PREYSPECCD)
    ) %>%
    left_join(pred_species, by = "pred_code") %>%
    left_join(prey_species, by = "prey_code") %>%
    mutate(
      stomach_content_wt = if_else(!is.na(STOWGT) & !is.na(EMPTYWGT), STOWGT - EMPTYWGT, NA_real_),
      has_prey_record = !is.na(prey_code)
    ) %>%
    select(
      set_seq = SET_SEQ,
      pred_seq = PRED_SEQ,
      prey_seq = PREY_SEQ,
      datasource = DATASOURCE,
      mission = MISSION,
      setno = SETNO,
      sdate = SDATE,
      year = year,
      month = month,
      strat = STRAT,
      bottom_temperature = BOTTOM_TEMPERATURE,
      depth = DEPTH,
      status_flag = STATUS_FLAG,
      gear = GEAR,
      slatdd = SLATDD,
      slongdd = SLONGDD,
      nafo_zone = NAFO_ZONE,
      nafo_subunit = NAFO_SUBUNIT,
      pred_code = pred_code,
      pred_common = pred_common,
      pred_latin = pred_latin,
      fshno = FSHNO,
      fwt = FWT,
      flen = FLEN,
      tech = TECH,
      stowgt = STOWGT,
      emptywgt = EMPTYWGT,
      stomach_content_wt = stomach_content_wt,
      fullness = FULLNESS,
      fgen = FGEN,
      fwt_calculated = FWT_CALCULATED,
      prey_code = prey_code,
      prey_common = prey_common,
      prey_latin = prey_latin,
      pwt = PWT,
      plen = PLEN,
      pnum = PNUM,
      rank = RANK,
      digestion = DIGESTION,
      remarks = REMARKS,
      preyvalue = PREYVALUE,
      has_prey_record = has_prey_record
    )

  list(
    stomach = food_habits_stomach,
    species_lookup = species_lookup
  )
}

# Run basic QA checks on key fields and code-join coverage, and print a
# compact QC summary to the console.
food_habits_qc <- function(food_habits_stomach, species_lookup) {
  prey_codes <- sort(unique(food_habits_stomach$prey_code[!is.na(food_habits_stomach$prey_code)]))
  pred_codes <- sort(unique(food_habits_stomach$pred_code[!is.na(food_habits_stomach$pred_code)]))
  species_codes <- sort(unique(species_lookup$species_code))

  unmatched_prey <- setdiff(prey_codes, species_codes)
  unmatched_pred <- setdiff(pred_codes, species_codes)

  message("Food habits QC summary")
  message("  rows: ", nrow(food_habits_stomach))
  message("  year range: ", min(food_habits_stomach$year, na.rm = TRUE), "-", max(food_habits_stomach$year, na.rm = TRUE))
  message("  missing prey in gut content (prey_code): ", sum(is.na(food_habits_stomach$prey_code)))
  message("  missing prey weight (pwt): ", sum(is.na(food_habits_stomach$pwt)))
  message("  unmatched predator codes: ", length(unmatched_pred))
  message("  unmatched prey codes: ", length(unmatched_prey))

  if (length(unmatched_prey) > 0) {
    message("  unmatched prey code examples: ", paste(utils::head(unmatched_prey, 10), collapse = ", "))
  }

  if (length(unmatched_pred) > 0) {
    warning("Some predator codes do not map to species dictionary.", call. = FALSE)
  }

  invisible(
    list(
      unmatched_prey_codes = unmatched_prey,
      unmatched_pred_codes = unmatched_pred
    )
  )
}

# Convert species common names to species codes for filtering/grouping.
lookup_species_codes <- function(species_lookup, common_names) {
  if (length(common_names) == 0) {
    return(integer())
  }

  species_lookup %>%
    filter(toupper(common_name) %in% toupper(common_names)) %>%
    pull(species_code) %>%
    unique() %>%
    sort()
}

# Filter stomach records by predator/prey codes and optional species groups.
filter_food_habits <- function(
    food_habits_stomach,
    predator_codes = NULL,
    prey_codes = NULL,
    predator_groups = NULL,
    prey_groups = NULL) {
  out <- food_habits_stomach

  if (!is.null(predator_groups) && length(predator_groups) > 0) {
    out <- out %>% filter(pred_code %in% unique(unlist(predator_groups)))
  }

  if (!is.null(prey_groups) && length(prey_groups) > 0) {
    out <- out %>% filter(prey_code %in% unique(unlist(prey_groups)))
  }

  if (!is.null(predator_codes) && length(predator_codes) > 0) {
    out <- out %>% filter(pred_code %in% predator_codes)
  }

  if (!is.null(prey_codes) && length(prey_codes) > 0) {
    out <- out %>% filter(prey_code %in% prey_codes)
  }

  out
}

# Summarize prey composition from a predator perspective, including prey
# proportions and within-group ranking by prey weight.
summarise_diet_composition <- function(food_habits_stomach, by = c("year", "nafo_zone", "pred_code", "pred_common")) {
  grouping <- intersect(by, names(food_habits_stomach))

  diet <- food_habits_stomach %>%
    filter(!is.na(prey_code), !is.na(pwt), pwt >= 0) %>%
    group_by(across(all_of(c(grouping, "prey_code", "prey_common")))) %>%
    summarise(
      prey_weight_total = sum(pwt, na.rm = TRUE),
      n_prey_records = dplyr::n(),
      n_stomachs = n_distinct(pred_seq),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(grouping))) %>%
    mutate(
      prey_weight_prop = prey_weight_total / sum(prey_weight_total, na.rm = TRUE),
      prey_rank = dplyr::dense_rank(dplyr::desc(prey_weight_total))
    ) %>%
    ungroup()

  diet
}

# Summarize predation pressure from a prey perspective, including predator
# proportions and within-group ranking by prey weight.
summarise_prey_pressure <- function(food_habits_stomach, by = c("year", "nafo_zone", "prey_code", "prey_common")) {
  grouping <- intersect(by, names(food_habits_stomach))

  food_habits_stomach %>%
    filter(!is.na(prey_code), !is.na(pwt), pwt >= 0) %>%
    group_by(across(all_of(c(grouping, "pred_code", "pred_common")))) %>%
    summarise(
      prey_weight_total = sum(pwt, na.rm = TRUE),
      n_prey_records = dplyr::n(),
      n_stomachs = n_distinct(pred_seq),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(grouping))) %>%
    mutate(
      predator_weight_prop = prey_weight_total / sum(prey_weight_total, na.rm = TRUE),
      predator_rank = dplyr::dense_rank(dplyr::desc(prey_weight_total))
    ) %>%
    ungroup()
}

# ---- Build data products ----

inputs <- load_food_habits_inputs(source_mode = source_mode)
std <- standardize_food_habits(
  stomach_raw = inputs$STOMACH_DATA_VW,
  species_raw = inputs$GSSPECIES
)

food_habits_stomach <- std$stomach
food_habits_species <- std$species_lookup

food_habits_qc(food_habits_stomach, food_habits_species)

# Priority species discussed with managers/assessment leads
priority_predators <- c(
  "ATLANTIC COD", "HADDOCK", "POLLOCK", "SILVER HAKE", "REDFISH", "AMERICAN PLAICE"
)
priority_prey <- c(
  "ATLANTIC HERRING", "NORTHERN SHRIMP", "GREEN SEA URCHIN", "SEA URCHIN"
)

priority_predator_codes <- lookup_species_codes(food_habits_species, priority_predators)
priority_prey_codes <- lookup_species_codes(food_habits_species, priority_prey)

food_habits_priority <- filter_food_habits(
  food_habits_stomach,
  predator_codes = priority_predator_codes
)

food_habits_diet_composition <- summarise_diet_composition(food_habits_priority)
food_habits_prey_pressure <- summarise_prey_pressure(
  filter_food_habits(food_habits_stomach, prey_codes = priority_prey_codes)
)

# Keep a package-facing object named `food_habits` for backwards continuity.
# This is now the detailed cleaned table rather than the previous CSV-derived format.
food_habits_full <- food_habits_stomach
attr(food_habits_full, "source_citation") <- "Cook and Bundy 2010; Mar.datawrangling extraction workflow"
attr(food_habits_full, "region") <- "Maritimes"

save(food_habits_full, file = here("data-raw", "food-habits", "food_habits_full.rda"))
save(food_habits_stomach, file = here("data-raw", "food-habits", "food_habits_stomach.rda"))
save(food_habits_species, file = here("data-raw", "food-habits", "food_habits_species.rda"))
save(food_habits_diet_composition, file = here("data-raw", "food-habits", "food_habits_diet_composition.rda"))
save(food_habits_prey_pressure, file = here("data-raw", "food-habits", "food_habits_prey_pressure.rda"))

usethis::use_data(
  food_habits_full,
  food_habits_stomach,
  food_habits_species,
  food_habits_diet_composition,
  food_habits_prey_pressure,
  overwrite = TRUE
)
