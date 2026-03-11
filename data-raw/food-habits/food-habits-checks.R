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

check_near <- function(x, y, tol = 1e-8) {
  isTRUE(all(abs(x - y) <= tol, na.rm = TRUE))
}

required_columns_present <- function(data, cols) {
  missing <- setdiff(cols, names(data))
  list(ok = length(missing) == 0, missing = missing)
}

composition_prop_check <- function(data, prop_var = "mean_diet_prop") {
  if (!(prop_var %in% names(data))) {
    return(list(ok = TRUE, max_sum = NA_real_, n_groups_over = 0L))
  }

  group_cols <- setdiff(
    names(data),
    c(
      "prey_code", "prey_common",
      "mean_diet_weight", "mean_diet_prop", "mean_occurrence_prop",
      "n_predators", "n_strata", "prey_rank"
    )
  )

  if (length(group_cols) == 0) {
    summed <- data.frame(sum_prop = sum(data[[prop_var]], na.rm = TRUE))
  } else {
    summed <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(sum_prop = sum(.data[[prop_var]], na.rm = TRUE), .groups = "drop")
  }

  tol <- 1e-8
  n_over <- sum(summed$sum_prop > (1 + tol), na.rm = TRUE)
  list(
    ok = n_over == 0,
    max_sum = max(summed$sum_prop, na.rm = TRUE),
    n_groups_over = n_over
  )
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
  prey_predation_group_vars,
  remove_excluded_codes = TRUE,
  excluded_prey_codes = food_habits_default_exclusion_prey_codes()
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
  mean_comp <- composition_prop_check(food_habits_mean_diet_stratified, prop_var = "mean_diet_prop")
  check_msg(
    mean_comp$ok,
    paste0("mean-diet composition sums <= 1 (max = ", signif(mean_comp$max_sum, 5), ", groups_over = ", mean_comp$n_groups_over, ")")
  )
  dom_comp <- composition_prop_check(food_habits_dominant_prey_timeseries, prop_var = "mean_diet_prop")
  check_msg(
    dom_comp$ok,
    paste0("dominant-prey composition sums <= 1 (max = ", signif(dom_comp$max_sum, 5), ", groups_over = ", dom_comp$n_groups_over, ")")
  )

  if (isTRUE(remove_excluded_codes) && "prey_code" %in% names(food_habits_mean_diet_stratified)) {
    check_msg(
      !any(food_habits_mean_diet_stratified$prey_code %in% excluded_prey_codes, na.rm = TRUE),
      "excluded prey codes are absent from mean-diet output"
    )
  }
  if (isTRUE(remove_excluded_codes) && "prey_code" %in% names(food_habits_prey_predation)) {
    check_msg(
      !any(food_habits_prey_predation$prey_code %in% excluded_prey_codes, na.rm = TRUE),
      "excluded prey codes are absent from predator-contribution output"
    )
  }

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
  fx_mean_comp <- composition_prop_check(fx_mean, prop_var = "mean_diet_prop")
  check_msg(fx_mean_comp$ok, "estimate_mean_diet() fixture composition sums <= 1")

  # Hand-calculated regression check:
  # verifies that prey absences are treated as implicit zeros during
  # length/strata weighted collapse (historical inflation bug guard).
  fx_manual <- data.frame(
    year = c(2020L, 2020L, 2020L, 2020L),
    strat = c("A", "A", "A", "B"),
    pred_seq = c(1L, 2L, 2L, 3L),
    pred_code = c(11L, 11L, 11L, 11L),
    pred_common = c("COD", "COD", "COD", "COD"),
    prey_code = c(60L, 60L, 2211L, 60L),
    prey_common = c("HERRING ATLANTIC", "HERRING ATLANTIC", "PANDALUS BOREALIS", "HERRING ATLANTIC"),
    pwt = c(1, 1, 1, 2),
    flen = c(5, 15, 15, 5),
    stringsAsFactors = FALSE
  )
  fx_manual_out <- estimate_mean_diet(
    food_habits_stomach = fx_manual,
    group_vars = c("pred_code"),
    length_breaks = c(0, 10, 20),
    include_label_cols = TRUE
  )

  manual_herring <- fx_manual_out |>
    dplyr::filter(prey_code == 60L) |>
    dplyr::pull(mean_diet_prop)
  manual_shrimp <- fx_manual_out |>
    dplyr::filter(prey_code == 2211L) |>
    dplyr::pull(mean_diet_prop)

  check_msg(length(manual_herring) == 1, "manual fixture has one herring row")
  check_msg(length(manual_shrimp) == 1, "manual fixture has one shrimp row")
  check_msg(check_near(manual_herring, 5 / 6, tol = 1e-6), "manual fixture herring mean_diet_prop == 5/6")
  check_msg(check_near(manual_shrimp, 1 / 6, tol = 1e-6), "manual fixture shrimp mean_diet_prop == 1/6")
  check_msg(check_near(manual_herring + manual_shrimp, 1, tol = 1e-6), "manual fixture prey proportions sum to 1")

  # Denominator mode regression checks:
  # verifies that denominator scope can be switched explicitly between
  # usable-prey predators and all sampled predators.
  fx_den <- data.frame(
    year = c(2020L, 2020L),
    strat = c("A", "A"),
    pred_seq = c(1L, 2L),
    pred_code = c(11L, 11L),
    pred_common = c("COD", "COD"),
    prey_code = c(NA_integer_, 60L),
    prey_common = c(NA_character_, "HERRING ATLANTIC"),
    pwt = c(NA_real_, 1),
    flen = c(30, 30),
    stringsAsFactors = FALSE
  )
  fx_den_usable <- estimate_mean_diet(
    food_habits_stomach = fx_den,
    group_vars = c("pred_code"),
    length_breaks = c(0, 50),
    include_label_cols = TRUE,
    remove_excluded_codes = FALSE,
    denominator_mode = "usable_predators"
  )
  fx_den_all <- estimate_mean_diet(
    food_habits_stomach = fx_den,
    group_vars = c("pred_code"),
    length_breaks = c(0, 50),
    include_label_cols = TRUE,
    remove_excluded_codes = FALSE,
    denominator_mode = "all_sampled_predators"
  )
  fx_den_usable_prop <- fx_den_usable |>
    dplyr::filter(prey_code == 60L) |>
    dplyr::pull(mean_diet_prop)
  fx_den_all_prop <- fx_den_all |>
    dplyr::filter(prey_code == 60L) |>
    dplyr::pull(mean_diet_prop)
  check_msg(length(fx_den_usable_prop) == 1, "denominator fixture usable mode has one prey row")
  check_msg(length(fx_den_all_prop) == 1, "denominator fixture all-sampled mode has one prey row")
  check_msg(check_near(fx_den_usable_prop, 1, tol = 1e-6), "denominator_mode='usable_predators' gives conditional proportion")
  check_msg(check_near(fx_den_all_prop, 0.5, tol = 1e-6), "denominator_mode='all_sampled_predators' includes empty predators")

  fx_den_excl <- data.frame(
    year = c(2020L, 2020L),
    strat = c("A", "A"),
    pred_seq = c(1L, 2L),
    pred_code = c(11L, 11L),
    pred_common = c("COD", "COD"),
    prey_code = c(9000L, 60L),
    prey_common = c("UNID REMAINS DIGESTED", "HERRING ATLANTIC"),
    pwt = c(1, 1),
    flen = c(30, 30),
    stringsAsFactors = FALSE
  )
  fx_den_excl_usable <- estimate_mean_diet(
    food_habits_stomach = fx_den_excl,
    group_vars = c("pred_code"),
    length_breaks = c(0, 50),
    include_label_cols = TRUE,
    remove_excluded_codes = TRUE,
    denominator_mode = "usable_predators"
  )
  fx_den_excl_all <- estimate_mean_diet(
    food_habits_stomach = fx_den_excl,
    group_vars = c("pred_code"),
    length_breaks = c(0, 50),
    include_label_cols = TRUE,
    remove_excluded_codes = TRUE,
    denominator_mode = "all_sampled_predators"
  )
  fx_den_excl_usable_prop <- fx_den_excl_usable |>
    dplyr::filter(prey_code == 60L) |>
    dplyr::pull(mean_diet_prop)
  fx_den_excl_all_prop <- fx_den_excl_all |>
    dplyr::filter(prey_code == 60L) |>
    dplyr::pull(mean_diet_prop)
  check_msg(check_near(fx_den_excl_usable_prop, 1, tol = 1e-6), "usable denominator excludes predators with only excluded prey")
  check_msg(check_near(fx_den_excl_all_prop, 0.5, tol = 1e-6), "all-sampled denominator retains predators with only excluded prey")

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

  fx_excl <- estimate_mean_diet(
    food_habits_stomach = dplyr::bind_rows(
      fx_stomach,
      dplyr::mutate(fx_stomach[1, ], prey_code = 9000L, prey_common = "UNID REMAINS DIGESTED")
    ),
    group_vars = c("pred_code"),
    include_label_cols = TRUE,
    remove_excluded_codes = TRUE
  )
  check_msg(!any(fx_excl$prey_code == 9000L, na.rm = TRUE), "excluded prey code (9000) removed in estimate_mean_diet()")

  # ---- Grouping variable sanity checks ----
  check_msg(length(existing_cols(food_habits_stomach, mean_diet_group_vars)) > 0, "mean_diet_group_vars resolve on data")
  check_msg(length(existing_cols(food_habits_stomach, dominant_prey_group_vars)) > 0, "dominant_prey_group_vars resolve on data")
  check_msg(length(existing_cols(food_habits_stomach, prey_predation_group_vars)) > 0, "prey_predation_group_vars resolve on data")

  message("=== All food-habits contract checks passed ===")
  invisible(TRUE)
}
