# Contract checks for food-habits data-raw workflow.
# These checks are intentionally outside package tests and are run from
# data-raw/food-habits/food-habits.R.

check_msg <- function(ok, label) {
  if (isTRUE(ok)) {
    message("[PASS] ", label)
    return(invisible(TRUE))
  }
  stop("[FAIL] ", label, call. = FALSE)
}

required_columns_present <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  list(ok = length(missing) == 0, missing = missing)
}

make_food_habits_fixture <- function() {
  stomach <- data.frame(
    year = c(2020L, 2020L, 2020L, 2020L, 2021L, 2021L),
    strat = c("A", "A", "A", "B", "B", "B"),
    pred_seq = c(1L, 1L, 2L, 3L, 4L, 4L),
    pred_code = c(11L, 11L, 11L, 14L, 14L, 14L),
    pred_common = c("COD", "COD", "COD", "HADDOCK", "HADDOCK", "HADDOCK"),
    prey_code = c(60L, 2211L, 60L, 2211L, 6400L, 2211L),
    prey_common = c("HERRING ATLANTIC", "PANDALUS BOREALIS", "HERRING ATLANTIC", "PANDALUS BOREALIS", "SEA URCHINS", "PANDALUS BOREALIS"),
    pwt = c(2, 1, 3, 4, 5, 1),
    flen = c(30, 30, 35, 40, 42, 42),
    stringsAsFactors = FALSE
  )

  species <- data.frame(
    species_code = c(11L, 14L, 60L, 2211L, 6400L),
    common_name = c("COD", "HADDOCK", "HERRING ATLANTIC", "PANDALUS BOREALIS", "SEA URCHINS"),
    latin_name = c("GADUS MORHUA", "MELANOGRAMMUS AEGLEFINUS", "CLUPEA HARENGUS", "PANDALUS BOREALIS", "ECHINOIDEA"),
    stringsAsFactors = FALSE
  )

  list(stomach = stomach, species = species)
}

run_food_habits_contract_checks <- function(
  food_habits_stomach,
  food_habits_species,
  food_habits_mean_diet_stratified,
  food_habits_dominant_prey_timeseries,
  food_habits_prey_predation,
  priority_predator_codes,
  priority_prey_codes,
  mean_diet_group_vars,
  dominant_prey_group_vars,
  prey_predation_group_vars
) {
  message("=== Food Habits Contract Checks ===")

  # ---- Real-data structure checks ----
  stomach_required <- c("year", "strat", "pred_seq", "pred_code", "prey_code", "pwt", "flen")
  species_required <- c("species_code", "common_name", "latin_name")
  mean_required <- c("mean_diet_weight", "mean_diet_prop", "mean_occurrence_prop", "prey_rank")
  dom_required <- c("mean_diet_weight", "mean_diet_prop", "mean_occurrence_prop", "prey_rank")
  pred_required <- c("prey_weight_total", "predator_weight_prop", "predator_rank")

  x <- required_columns_present(food_habits_stomach, stomach_required)
  check_msg(x$ok, paste0("food_habits_stomach has required columns (missing: ", paste(x$missing, collapse = ", "), ")"))
  x <- required_columns_present(food_habits_species, species_required)
  check_msg(x$ok, paste0("food_habits_species has required columns (missing: ", paste(x$missing, collapse = ", "), ")"))
  x <- required_columns_present(food_habits_mean_diet_stratified, mean_required)
  check_msg(x$ok, paste0("food_habits_mean_diet_stratified has required columns (missing: ", paste(x$missing, collapse = ", "), ")"))
  x <- required_columns_present(food_habits_dominant_prey_timeseries, dom_required)
  check_msg(x$ok, paste0("food_habits_dominant_prey_timeseries has required columns (missing: ", paste(x$missing, collapse = ", "), ")"))
  x <- required_columns_present(food_habits_prey_predation, pred_required)
  check_msg(x$ok, paste0("food_habits_prey_predation has required columns (missing: ", paste(x$missing, collapse = ", "), ")"))

  check_msg(nrow(food_habits_stomach) > 0, "food_habits_stomach has rows")
  check_msg(nrow(food_habits_mean_diet_stratified) > 0, "mean diet output has rows")
  check_msg(nrow(food_habits_dominant_prey_timeseries) > 0, "dominant prey output has rows")
  check_msg(nrow(food_habits_prey_predation) > 0, "predator contribution output has rows")

  check_msg(length(priority_predator_codes) > 0, "priority_predator_codes resolved")
  check_msg(length(priority_prey_codes) > 0, "priority_prey_codes resolved")

  check_msg(all(food_habits_mean_diet_stratified$mean_diet_weight >= 0, na.rm = TRUE), "mean_diet_weight is non-negative")
  check_msg(all(food_habits_mean_diet_stratified$mean_diet_prop >= 0 & food_habits_mean_diet_stratified$mean_diet_prop <= 1, na.rm = TRUE), "mean_diet_prop is in [0,1]")
  check_msg(all(food_habits_prey_predation$predator_weight_prop >= 0 & food_habits_prey_predation$predator_weight_prop <= 1, na.rm = TRUE), "predator_weight_prop is in [0,1]")

  # ---- Method behavior checks on synthetic fixture ----
  fx <- make_food_habits_fixture()
  fx_stomach <- fx$stomach
  fx_species <- fx$species

  fx_mean <- estimate_mean_diet(
    food_habits_stomach = fx_stomach,
    group_vars = c("year", "pred_code"),
    include_label_cols = TRUE
  )
  check_msg(nrow(fx_mean) > 0, "estimate_mean_diet() returns rows on fixture")
  check_msg(all(fx_mean$mean_diet_weight >= 0, na.rm = TRUE), "estimate_mean_diet() fixture weights are non-negative")

  fx_dom <- estimate_dominant_prey(
    food_habits_stomach = fx_stomach,
    group_vars = c("year", "pred_code"),
    top_n = 1,
    include_label_cols = TRUE
  )
  dom_groups <- existing_cols(fx_dom, c("year", "pred_code", "pred_common"))
  dom_max_n <- fx_dom |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dom_groups))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::summarise(max_n = max(n), .groups = "drop") |>
    dplyr::pull(max_n)
  check_msg(all(fx_dom$prey_rank <= 1), "estimate_dominant_prey(top_n=1) keeps prey_rank <= 1")
  check_msg(length(dom_max_n) == 1 && dom_max_n <= 1, "estimate_dominant_prey(top_n=1) keeps <=1 prey per group")

  fx_pred <- estimate_predator_contribution(
    food_habits_stomach = fx_stomach,
    group_vars = c("year", "prey_code"),
    top_n_predators = 1,
    include_label_cols = TRUE
  )
  pred_groups <- existing_cols(fx_pred, c("year", "prey_code", "prey_common"))
  pred_max_n <- fx_pred |>
    dplyr::group_by(dplyr::across(dplyr::all_of(pred_groups))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::summarise(max_n = max(n), .groups = "drop") |>
    dplyr::pull(max_n)
  check_msg(all(fx_pred$predator_rank <= 1), "estimate_predator_contribution(top_n_predators=1) keeps predator_rank <= 1")
  check_msg(length(pred_max_n) == 1 && pred_max_n <= 1, "estimate_predator_contribution(top_n_predators=1) keeps <=1 predator per group")

  resolved_codes <- resolve_priority_codes(
    species_lookup = fx_species,
    common_names = c("ATLANTIC HERRING", "PANDALUS BOREALIS", "SEA URCHIN")
  )
  check_msg(all(c(60L, 2211L, 6400L) %in% resolved_codes), "resolve_priority_codes() handles exact and token-based matches on fixture")

  # ---- Grouping variable sanity checks ----
  check_msg(length(existing_cols(food_habits_stomach, mean_diet_group_vars)) > 0, "mean_diet_group_vars resolve on data")
  check_msg(length(existing_cols(food_habits_stomach, dominant_prey_group_vars)) > 0, "dominant_prey_group_vars resolve on data")
  check_msg(length(existing_cols(food_habits_stomach, prey_predation_group_vars)) > 0, "prey_predation_group_vars resolve on data")

  message("=== All food-habits contract checks passed ===")
  invisible(TRUE)
}
