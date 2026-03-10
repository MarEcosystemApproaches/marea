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
load_food_habits_inputs <- function(source_mode = c("local_rdata", "mar_datawrangling")) {
  source_mode <- match.arg(source_mode)

  if (identical(source_mode, "mar_datawrangling")) {
    # TODO(client): Replace this placeholder with authenticated Mar.datawrangling code.
    stop(
      "source_mode = 'mar_datawrangling' is a placeholder. ",
      "Client-provided authenticated retrieval code is still required.",
      call. = FALSE
    )
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
  species_lookup <- species_raw |>
    dplyr::mutate(
      species_code = as.integer(CODE),
      common_name = stringr::str_squish(as.character(COMM)),
      latin_name = stringr::str_squish(as.character(SPEC))
    ) |>
    dplyr::select(species_code, common_name, latin_name)

  pred_species <- species_lookup |>
    dplyr::rename(
      pred_code = species_code,
      pred_common = common_name,
      pred_latin = latin_name
    )

  prey_species <- species_lookup |>
    dplyr::rename(
      prey_code = species_code,
      prey_common = common_name,
      prey_latin = latin_name
    )

  food_habits_stomach <- stomach_raw |>
    dplyr::mutate(
      SDATE = as.Date(SDATE),
      year = as.integer(format(SDATE, "%Y")),
      month = as.integer(format(SDATE, "%m")),
      pred_code = as.integer(SPEC),
      prey_code = as.integer(PREYSPECCD)
    ) |>
    dplyr::left_join(pred_species, by = "pred_code") |>
    dplyr::left_join(prey_species, by = "prey_code") |>
    dplyr::mutate(
      stomach_content_wt = dplyr::if_else(!is.na(STOWGT) & !is.na(EMPTYWGT), STOWGT - EMPTYWGT, NA_real_),
      has_prey_record = !is.na(prey_code)
    ) |>
    janitor::clean_names() |>
    dplyr::select(
      !c("stime", "spec", "preyspeccd")
    )

  list(
    stomach = food_habits_stomach,
    species_lookup = species_lookup
  )
}

# Run basic QA checks on key fields and code-join coverage, and print a
# compact QC summary to the console.
food_habits_qc <- function(food_habits_stomach, species_lookup) {
  prey_codes <- unique(food_habits_stomach$prey_code[!is.na(food_habits_stomach$prey_code)]) |>
    sort()
  pred_codes <- unique(food_habits_stomach$pred_code[!is.na(food_habits_stomach$pred_code)]) |>
    sort()
  species_codes <- unique(species_lookup$species_code) |>
    sort()

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

# Map code columns to default human-readable label columns.
food_habits_default_label_map <- function() {
  c(
    pred_code = "pred_common",
    prey_code = "prey_common"
  )
}

# Default prey codes to exclude from diet-focused products.
# Includes parasites, highly digested/unidentified remains, and non-diet
# artefacts (e.g., mud, water, stones). Code 1099 (fish remains) is included
# with 9002 to avoid duplication in current coding practice.
food_habits_default_exclusion_prey_codes <- function() {
  as.integer(c(
    1099,
    7000, 7100, 7101, 7110, 7111, 7113, 7116,
    9000, 9001, 9002, 9003,
    9100, 9200, 9500, 9600, 9650, 9700, 9800
  ))
}

# Conditionally remove prey codes from stomach records before analysis.
apply_prey_code_exclusions <- function(
    food_habits_stomach,
    prey_var = "prey_code",
    remove_excluded_codes = TRUE,
    excluded_prey_codes = food_habits_default_exclusion_prey_codes()) {
  if (!isTRUE(remove_excluded_codes)) {
    return(food_habits_stomach)
  }
  if (!(prey_var %in% names(food_habits_stomach))) {
    stop("prey_var column not found in food_habits_stomach: ", prey_var, call. = FALSE)
  }
  if (length(excluded_prey_codes) == 0) {
    return(food_habits_stomach)
  }

  food_habits_stomach %>%
    dplyr::filter(is.na(.data[[prey_var]]) | !(.data[[prey_var]] %in% excluded_prey_codes))
}

# Convert species common names to species codes for filtering/grouping.
lookup_species_codes <- function(species_lookup, common_names) {
  if (length(common_names) == 0) {
    return(integer())
  }

  species_lookup |>
    dplyr::filter(toupper(common_name) %in% toupper(common_names)) |>
    dplyr::pull(species_code) |>
    unique() |>
    sort()
}

# Filter stomach records by predator/prey codes and optional species groups.
filter_food_habits <- function(
  food_habits_stomach,
  predator_codes = NULL,
  prey_codes = NULL,
  predator_groups = NULL,
  prey_groups = NULL
) {
  out <- food_habits_stomach

  if (!is.null(predator_groups) && length(predator_groups)) {
    out <- out |> dplyr::filter(pred_code %in% unique(unlist(predator_groups)))
  }

  if (!is.null(prey_groups) && length(prey_groups)) {
    out <- out |> dplyr::filter(prey_code %in% unique(unlist(prey_groups)))
  }

  if (!is.null(predator_codes) && length(predator_codes)) {
    out <- out |> dplyr::filter(pred_code %in% predator_codes)
  }

  if (!is.null(prey_codes) && length(prey_codes)) {
    out <- out |> dplyr::filter(prey_code %in% prey_codes)
  }

  out
}

# Return existing columns from a proposed character vector.
existing_cols <- function(x, cols) {
  intersect(cols, names(x))
}

# For any code variable in grouping, include mapped label columns if they exist.
add_label_cols <- function(data, group_vars, label_map = food_habits_default_label_map()) {
  out <- unique(group_vars)
  for (g in group_vars) {
    if (g %in% names(label_map)) {
      label_col <- unname(label_map[[g]])
      if (label_col %in% names(data)) {
        out <- unique(c(out, label_col))
      }
    }
  }
  out
}

# Resolve priority common names to species codes.
# Strategy: exact match first; fallback to token-based matching robust to
# word order (e.g., "ATLANTIC HERRING" vs "HERRING ATLANTIC").
resolve_priority_codes <- function(species_lookup, common_names) {
  out <- integer(0)

  for (nm in common_names) {
    nm_upper <- toupper(stringr::str_squish(nm))

    exact <- species_lookup |>
      dplyr::filter(toupper(common_name) == nm_upper) |>
      dplyr::pull(species_code) |>
      unique()

    if (length(exact) > 0) {
      out <- c(out, exact[1])
      next
    }

    tokens <- unlist(strsplit(gsub("[^A-Z0-9 ]", " ", nm_upper), "\\s+"))
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0) {
      next
    }

    token_hits <- species_lookup |>
      dplyr::filter(!is.na(common_name)) |>
      dplyr::mutate(common_upper = toupper(common_name)) |>
      dplyr::filter(vapply(
        common_upper,
        function(x) all(vapply(tokens, grepl, logical(1), x = x, fixed = TRUE)),
        logical(1)
      )) |>
      dplyr::pull(species_code) |>
      unique()

    if (length(token_hits) > 0) {
      out <- c(out, token_hits[1])
    }
  }

  sort(unique(out))
}

sanitize_plot_suffix <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

save_plot_file <- function(plot_obj, out_dir, file_stub) {
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0(file_stub, ".png")),
    plot = plot_obj,
    width = 10,
    height = 7,
    dpi = 300
  )
}

# Export food-habits summary plots either as one plot per species of interest
# or one aggregated plot per output.
export_food_habits_plots <- function(
  food_habits_mean_diet_stratified,
  food_habits_dominant_prey_timeseries,
  food_habits_prey_predation,
  food_habits_species,
  priority_predator_codes,
  priority_prey_codes,
  plot_export_mode = "per_species",
  out_dir = file.path("data-raw", "food-habits")
) {
  if (!dir.exists(out_dir)) {
    stop("Output directory does not exist: ", out_dir, call. = FALSE)
  }

  per_species_pattern <- "^food_habits_(mean_diet_stratified|dominant_prey_timeseries|prey_predation)_.+[.]png$"
  aggregated_files <- file.path(
    out_dir,
    c(
      "food_habits_mean_diet_stratified.png",
      "food_habits_dominant_prey_timeseries.png",
      "food_habits_prey_predation.png"
    )
  )

  if (identical(plot_export_mode, "per_species")) {
    old_species_plots <- list.files(out_dir, pattern = per_species_pattern, full.names = TRUE)
    if (length(old_species_plots) > 0) {
      unlink(old_species_plots)
    }
    unlink(aggregated_files[file.exists(aggregated_files)])

    if (length(priority_predator_codes) == 0) {
      stop(
        "No priority predator codes resolved. ",
        "Check values in priority_predators against food_habits_species$common_name.",
        call. = FALSE
      )
    }
    if (length(priority_prey_codes) == 0) {
      stop(
        "No priority prey codes resolved. ",
        "Check values in priority_prey against food_habits_species$common_name.",
        call. = FALSE
      )
    }

    species_labels <- food_habits_species |>
      dplyr::select(species_code, common_name) |>
      dplyr::rename(code = species_code, label = common_name)

    for (code in priority_predator_codes) {
      code <- as.integer(code)
      sp_label <- species_labels$label[match(code, species_labels$code)][1]
      if (is.na(sp_label) || length(sp_label) == 0) {
        sp_label <- as.character(code)
      }
      sp_suffix <- sanitize_plot_suffix(sp_label)
      md_dat <- food_habits_mean_diet_stratified |>
        dplyr::filter(pred_code == {{ code }})
      dp_dat <- food_habits_dominant_prey_timeseries |>
        dplyr::filter(pred_code == {{ code }})

      if (nrow(md_dat)) {
        save_plot_file(
          plot_mean_diet(md_dat),
          out_dir,
          paste0("food_habits_mean_diet_stratified_", sp_suffix)
        )
      }

      if (nrow(dp_dat)) {
        save_plot_file(
          plot_dominant_prey(dp_dat, facet_by_predator = FALSE),
          out_dir,
          paste0("food_habits_dominant_prey_timeseries_", sp_suffix)
        )
      }
    }

    for (code in priority_prey_codes) {
      code <- as.integer(code)
      sp_label <- species_labels$label[match(code, species_labels$code)][1]
      if (is.na(sp_label) || length(sp_label) == 0) {
        sp_label <- as.character(code)
      }
      sp_suffix <- sanitize_plot_suffix(sp_label)

      pc_dat <- food_habits_prey_predation |>
        dplyr::filter(prey_code == {{ code }})
      if (nrow(pc_dat) > 0) {
        save_plot_file(
          plot_predator_contribution(pc_dat, facet_by_prey = FALSE),
          out_dir,
          paste0("food_habits_prey_predation_", sp_suffix)
        )
      }
    }
    return(invisible(NULL))
  }

  if (identical(plot_export_mode, "aggregated")) {
    old_species_plots <- list.files(out_dir, pattern = per_species_pattern, full.names = TRUE)
    if (length(old_species_plots) > 0) {
      unlink(old_species_plots)
    }

    save_plot_file(
      plot_mean_diet(food_habits_mean_diet_stratified),
      out_dir,
      "food_habits_mean_diet_stratified"
    )
    save_plot_file(
      plot_dominant_prey(food_habits_dominant_prey_timeseries, facet_by_predator = FALSE),
      out_dir,
      "food_habits_dominant_prey_timeseries"
    )
    save_plot_file(
      plot_predator_contribution(food_habits_prey_predation, facet_by_prey = FALSE),
      out_dir,
      "food_habits_prey_predation"
    )
    return(invisible(NULL))
  }

  stop("plot_export_mode must be either 'per_species' or 'aggregated'.", call. = FALSE)
}

# Export unaggregated mean-diet example figures (by strata and by length),
# one figure per predator species.
export_food_habits_mean_diet_unaggregated_plots <- function(
  food_habits_mean_diet_by_strata_example,
  food_habits_mean_diet_by_length_example,
  out_dir = file.path("data-raw", "food-habits"),
  top_n = 12,
  length_bin_var = "length_bin"
) {
  if (!dir.exists(out_dir)) {
    stop("Output directory does not exist: ", out_dir, call. = FALSE)
  }

  old_files <- list.files(
    out_dir,
    pattern = "^food_habits_mean_diet_by_(strata|length)_.+[.]png$",
    full.names = TRUE
  )
  if (length(old_files) > 0) {
    unlink(old_files)
  }

  pred_species_col <- if ("pred_common" %in% names(food_habits_mean_diet_by_strata_example)) {
    "pred_common"
  } else {
    "pred_code"
  }

  pred_species_values <- food_habits_mean_diet_by_strata_example |>
    dplyr::filter(!is.na(.data[[pred_species_col]])) |>
    dplyr::pull(.data[[pred_species_col]]) |>
    unique()

  for (sp in pred_species_values) {
    sp_suffix <- sanitize_plot_suffix(sp)

    strata_dat <- food_habits_mean_diet_by_strata_example |>
      dplyr::filter(.data[[pred_species_col]] == sp)
    length_dat <- food_habits_mean_diet_by_length_example |>
      dplyr::filter(.data[[pred_species_col]] == sp)

    if (nrow(strata_dat)) {
      save_plot_file(
        plot_mean_diet_by_strata(strata_dat, top_n = top_n),
        out_dir,
        paste0("food_habits_mean_diet_by_strata_", sp_suffix)
      )
    }

    if (nrow(length_dat)) {
      save_plot_file(
        plot_mean_diet_by_length(length_dat, top_n = top_n, length_bin_var = length_bin_var),
        out_dir,
        paste0("food_habits_mean_diet_by_length_", sp_suffix)
      )
    }
  }

  invisible(NULL)
}
