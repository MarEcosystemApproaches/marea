#' Stratified Mean Diet Estimator by Area and Predator Length
#'
#' Estimate mean diet composition under a stratified survey framework by combining
#' prey weights at predator level, then aggregating across predator length strata
#' and survey strata with configurable weights.
#'
#' @description
#' `estimate_mean_diet()` provides a flexible, design-aware estimator
#' for food-habits summaries. The method first computes predator-level diet
#' quantities, then applies two-stage aggregation:
#' 1) within-stratum aggregation across predator length classes, and
#' 2) across-strata aggregation to produce population-level estimates.
#'
#' The function supports custom grouping dimensions (for example `year`,
#' `mission`, `nafo_zone`, `strat`, `pred_code`) and optional external weights
#' for both length and stratum levels.
#'
#' @details
#' Method steps:
#' 1. Validate requested grouping and prey columns; optionally append readable
#' label columns (for example `pred_common` when `pred_code` is present).
#' 2. Define length strata either from an existing `length_bin_var` or by
#' binning predator length (`length_var`) using `length_bin_width` or `length_breaks`.
#' 3. Within each predator (`predator_id_var`) and group/stratum/length bin:
#' compute prey-specific consumed weight and total consumed weight.
#' 4. Compute predator-level prey proportion as
#' `prey_weight / predator_total_weight`.
#' 5. Within each group/stratum/length/prey combination, estimate:
#'   - mean prey weight per predator
#'   - mean prey proportion per predator
#'   - prey occurrence proportion among predators
#' 6. Collapse across length bins within each stratum using `length_weights` if
#' provided, otherwise sample-based predator counts.
#' 7. Collapse across strata using `strata_weights` if provided, otherwise
#' sample-based predator counts.
#' 8. Rank prey within each group by descending estimated mean diet weight.
#'
#' Design rationale:
#' - Length-stratified stomach sampling is represented explicitly by estimating
#' prey metrics at predator-length level before stratum collapse.
#' - Stratified survey structure is represented by stratum-level aggregation with
#' explicit stratum weights.
#' - Sample-count defaults provide a transparent interim weighting option when
#' external survey design weights are not yet available.
#'
#' Assumptions:
#' - `weight_var` contains non-negative prey weight values.
#' - `predator_id_var` uniquely identifies predators/stomachs for averaging.
#' - If external weights are omitted, sample-count weighting is acceptable for
#' the intended application.
#'
#' @param food_habits_stomach A standardized stomach-prey table.
#' @param group_vars Character vector of grouping columns for reported estimates.
#' @param prey_var Character scalar naming the prey category column.
#' @param weight_var Character scalar naming prey-weight values.
#' @param predator_id_var Character scalar naming predator/stomach ID.
#' @param strata_var Character scalar naming survey stratum.
#' @param length_var Character scalar naming predator length variable.
#' @param length_bin_width Numeric scalar; width of auto-generated length bins.
#' @param length_breaks Optional numeric vector of explicit `cut()` breakpoints.
#' @param length_bin_var Optional character scalar naming an existing length-bin
#' column. If `NULL`, bins are created from `length_var`.
#' @param include_label_cols Logical; if `TRUE`, add mapped label columns for
#' grouped code fields.
#' @param label_map Named character vector mapping code columns to label columns.
#' @param length_weights Optional table of length-level weights.
#' @param length_weight_var Character scalar naming the length-weight column in
#' `length_weights`.
#' @param strata_weights Optional table of strata-level weights.
#' @param strata_weight_var Character scalar naming the strata-weight column in
#' `strata_weights`.
#' @param remove_excluded_codes Logical. If `TRUE`, remove excluded prey codes
#' (parasites, highly digested remains, artefacts) before estimation.
#' @param excluded_prey_codes Integer vector of prey codes to exclude when
#' `remove_excluded_codes = TRUE`.
#' @param retain_strata Logical. If `TRUE`, keep survey strata separated in
#' output instead of collapsing across strata.
#' @param retain_length_bins Logical. If `TRUE`, keep predator length bins
#' separated in output instead of collapsing across length bins.
#'
#' @return A tibble with one row per group-by-prey combination, including:
#' \describe{
#'   \item{mean_diet_weight}{Stratified mean prey weight}
#'   \item{mean_diet_prop}{Stratified mean prey proportion}
#'   \item{mean_occurrence_prop}{Stratified prey occurrence proportion}
#'   \item{n_predators}{Number of predators contributing after aggregation}
#'   \item{n_strata}{Number of strata contributing}
#'   \item{prey_rank}{Within-group rank by descending `mean_diet_weight`}
#' }
#'
#' @examples
#' # Example call pattern (assuming food_habits_stomach exists):
#' # estimate_mean_diet(
#' #   food_habits_stomach = food_habits_stomach,
#' #   group_vars = c("year", "strat", "pred_code"),
#' #   length_bin_width = 5
#' # )
#'
#' @export
estimate_mean_diet <- function(
    food_habits_stomach,
    group_vars = c("year", "strat", "pred_code"),
    prey_var = "prey_code",
    weight_var = "pwt",
    predator_id_var = "pred_seq",
    strata_var = "strat",
    length_var = "flen",
    length_bin_width = 5,
    length_breaks = NULL,
    length_bin_var = NULL,
    include_label_cols = TRUE,
    label_map = food_habits_default_label_map(),
    length_weights = NULL,
    length_weight_var = "weight",
    strata_weights = NULL,
    strata_weight_var = "weight",
    remove_excluded_codes = TRUE,
    excluded_prey_codes = food_habits_default_exclusion_prey_codes(),
    retain_strata = FALSE,
    retain_length_bins = FALSE) {
  group_vars <- existing_cols(food_habits_stomach, group_vars)
  prey_var <- existing_cols(food_habits_stomach, prey_var)

  if (length(group_vars) == 0) {
    stop("group_vars must contain at least one existing column.", call. = FALSE)
  }
  if (length(prey_var) != 1) {
    stop("prey_var must resolve to one existing column.", call. = FALSE)
  }

  food_habits_stomach <- apply_prey_code_exclusions(
    food_habits_stomach = food_habits_stomach,
    prey_var = prey_var,
    remove_excluded_codes = remove_excluded_codes,
    excluded_prey_codes = excluded_prey_codes
  )

  if (include_label_cols) {
    group_vars <- add_label_cols(food_habits_stomach, group_vars, label_map = label_map)
    prey_group_vars <- add_label_cols(food_habits_stomach, prey_var, label_map = label_map)
  } else {
    prey_group_vars <- prey_var
  }

  if (is.null(length_bin_var)) {
    dat <- food_habits_stomach |>
      dplyr::mutate(length_bin = build_length_bins(.data[[length_var]], bin_width = length_bin_width, breaks = length_breaks))
    length_bin_var <- "length_bin"
  } else {
    dat <- food_habits_stomach
  }

  dat <- dat |>
    dplyr::filter(
      !is.na(.data[[predator_id_var]]),
      !is.na(.data[[strata_var]]),
      !is.na(.data[[length_bin_var]]),
      !is.na(.data[[prey_var]]),
      !is.na(.data[[weight_var]]),
      .data[[weight_var]] >= 0
    )

  pred_keys <- unique(c(group_vars, strata_var, length_bin_var, predator_id_var))
  prey_keys <- unique(c(pred_keys, prey_group_vars))

  predator_totals <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(pred_keys))) |>
    dplyr::summarise(pred_total_weight = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop")

  prey_by_predator <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(prey_keys))) |>
    dplyr::summarise(prey_weight = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(predator_totals, by = pred_keys) |>
    dplyr::mutate(
      prey_prop = dplyr::if_else(pred_total_weight > 0, prey_weight / pred_total_weight, NA_real_)
    )

  pred_counts <- predator_totals |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(unique(c(group_vars, strata_var, length_bin_var)))
      )
    ) |>
    dplyr::summarise(
      n_predators_total = dplyr::n_distinct(.data[[predator_id_var]]),
      .groups = "drop"
    )

  prey_length_stratum <- prey_by_predator |>
    dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(group_vars, strata_var, length_bin_var, prey_group_vars))))) |>
    dplyr::summarise(
      sum_prey_weight = sum(prey_weight, na.rm = TRUE),
      sum_prey_prop = sum(prey_prop, na.rm = TRUE),
      n_predators_with_prey = dplyr::n_distinct(.data[[predator_id_var]]),
      .groups = "drop"
    ) |>
    dplyr::left_join(pred_counts, by = unique(c(group_vars, strata_var, length_bin_var))) |>
    dplyr::mutate(
      mean_prey_weight = sum_prey_weight / n_predators_total,
      mean_prey_prop = sum_prey_prop / n_predators_total,
      prey_occurrence_prop = n_predators_with_prey / n_predators_total
    )

  # if strata/length are explicitly requested in
  # group_vars, keep them in final output even if retain_* flags are FALSE.
  retain_strata <- isTRUE(retain_strata) || strata_var %in% group_vars
  retain_length_bins <- isTRUE(retain_length_bins) || length_bin_var %in% group_vars

  if (is.null(length_weights)) {
    length_weight_tbl <- pred_counts |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(unique(c(group_vars, strata_var, length_bin_var)))),
        length_weight = n_predators_total
      )
  } else {
    length_weight_keys <- existing_cols(length_weights, unique(c(group_vars, strata_var, length_bin_var)))
    length_weight_tbl <- length_weights |>
      dplyr::select(dplyr::all_of(c(length_weight_keys, length_weight_var))) |>
      dplyr::rename(length_weight = dplyr::all_of(length_weight_var))
  }

  if (retain_length_bins) {
    level_prey <- prey_length_stratum |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(unique(c(group_vars, strata_var, length_bin_var, prey_group_vars)))),
        level_mean_prey_weight = mean_prey_weight,
        level_mean_prey_prop = mean_prey_prop,
        level_mean_occurrence = prey_occurrence_prop,
        n_predators_level = n_predators_total
      )
  } else {
    level_prey <- prey_length_stratum |>
      dplyr::left_join(length_weight_tbl, by = unique(c(group_vars, strata_var, length_bin_var))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(group_vars, strata_var, prey_group_vars))))) |>
      dplyr::summarise(
        level_mean_prey_weight = safe_weighted_mean(mean_prey_weight, length_weight),
        level_mean_prey_prop = safe_weighted_mean(mean_prey_prop, length_weight),
        level_mean_occurrence = safe_weighted_mean(prey_occurrence_prop, length_weight),
        n_predators_level = sum(n_predators_total, na.rm = TRUE),
        .groups = "drop"
      )
  }

  if (retain_strata) {
    final_dat <- level_prey |>
      dplyr::transmute(
        dplyr::across(dplyr::all_of(unique(c(group_vars, strata_var, if (retain_length_bins) length_bin_var, prey_group_vars)))),
        mean_diet_weight = level_mean_prey_weight,
        mean_diet_prop = level_mean_prey_prop,
        mean_occurrence_prop = level_mean_occurrence,
        n_predators = n_predators_level,
        n_strata = 1L
      )
  } else {
    strata_weight_keys <- unique(c(group_vars, if (retain_length_bins) length_bin_var, strata_var))
    if (is.null(strata_weights)) {
      strata_weight_tbl <- pred_counts |>
        dplyr::group_by(dplyr::across(dplyr::all_of(strata_weight_keys))) |>
        dplyr::summarise(strata_weight = sum(n_predators_total, na.rm = TRUE), .groups = "drop")
    } else {
      strata_weight_keys_in <- existing_cols(strata_weights, strata_weight_keys)
      strata_weight_tbl <- strata_weights |>
        dplyr::select(dplyr::all_of(c(strata_weight_keys_in, strata_weight_var))) |>
        dplyr::rename(strata_weight = dplyr::all_of(strata_weight_var))
    }

    final_dat <- level_prey |>
      dplyr::left_join(strata_weight_tbl, by = strata_weight_keys) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(group_vars, if (retain_length_bins) length_bin_var, prey_group_vars))))) |>
      dplyr::summarise(
        mean_diet_weight = safe_weighted_mean(level_mean_prey_weight, strata_weight),
        mean_diet_prop = safe_weighted_mean(level_mean_prey_prop, strata_weight),
        mean_occurrence_prop = safe_weighted_mean(level_mean_occurrence, strata_weight),
        n_predators = sum(n_predators_level, na.rm = TRUE),
        n_strata = dplyr::n_distinct(.data[[strata_var]]),
        .groups = "drop"
      )
  }

  rank_group_vars <- unique(c(group_vars, if (retain_strata) strata_var, if (retain_length_bins) length_bin_var))

  out <- final_dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(rank_group_vars))) |>
    dplyr::mutate(
      prey_rank = dplyr::dense_rank(dplyr::desc(mean_diet_weight))
    ) |>
    dplyr::ungroup()

  out
}

# --------------------------------------------------------------------------------
# utils

# build_length_bins()
# Purpose:
#   Create predator length classes used for length-stratified estimation.
# Behavior:
#   - If `breaks` are provided, use them directly with `cut()`.
#   - Otherwise create regular bins from min/max observed length using `bin_width`.
#   - Return a character vector of bin labels (e.g., "[30,35)").
# Why:
#   Supports length-stratified stomach sampling by assigning each predator to a
#   length stratum before weighted aggregation.
build_length_bins <- function(x, bin_width = 5, breaks = NULL, right = FALSE) {
  if (!is.null(breaks)) {
    return(as.character(cut(x, breaks = breaks, include.lowest = TRUE, right = right)))
  }

  if (all(is.na(x))) {
    return(rep(NA_character_, length(x)))
  }

  lo <- floor(min(x, na.rm = TRUE) / bin_width) * bin_width
  hi <- ceiling(max(x, na.rm = TRUE) / bin_width) * bin_width
  if (lo == hi) {
    hi <- lo + bin_width
  }
  auto_breaks <- seq(lo, hi, by = bin_width)
  as.character(cut(x, breaks = auto_breaks, include.lowest = TRUE, right = right))
}

# safe_weighted_mean()
# Purpose:
#   Compute a weighted mean robustly during multi-stage aggregation.
# Behavior:
#   - Remove rows with missing values in either `x` or `w`.
#   - Return `NA` when no valid rows remain or total weight is <= 0.
#   - Otherwise compute sum(x * w) / sum(w).
# Why:
#   Prevents divide-by-zero and empty-group failures in length- and
#   stratum-weighted summaries.
safe_weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) {
    return(NA_real_)
  }
  sw <- sum(w[ok])
  if (sw <= 0) {
    return(NA_real_)
  }
  sum(x[ok] * w[ok]) / sw
}

# Plot summary for mean-diet estimates: top prey by aggregated mean diet weight.
plot_mean_diet <- function(mean_diet_data, top_n = 12) {
  prey_label_var <- if ("prey_common" %in% names(mean_diet_data)) "prey_common" else "prey_code"

  plot_dat <- mean_diet_data |>
    dplyr::filter(!is.na(.data[[prey_label_var]]), !is.na(mean_diet_weight)) |>
    dplyr::group_by(.data[[prey_label_var]]) |>
    dplyr::summarise(plot_value = sum(mean_diet_weight, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(plot_value)) |>
    dplyr::slice_head(n = top_n)

  ggplot2::ggplot(plot_dat, ggplot2::aes(x = stats::reorder(.data[[prey_label_var]], plot_value), y = plot_value)) +
    ggplot2::geom_col(fill = "#2C6E91") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Mean Diet: Top Prey",
      subtitle = "Aggregated weighted mean prey weight across selected groups",
      x = "Prey",
      y = "Aggregated mean diet weight"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

# Plot unaggregated mean-diet output by survey strata as a prey x strata bubble plot.
# Color encodes mean_diet_prop, bubble size encodes mean_diet_weight.
plot_mean_diet_by_strata <- function(mean_diet_data, top_n = 12) {
  prey_label_var <- if ("prey_common" %in% names(mean_diet_data)) "prey_common" else "prey_code"
  if (!("strat" %in% names(mean_diet_data))) {
    stop("plot_mean_diet_by_strata() requires a 'strat' column.", call. = FALSE)
  }
  if (!all(c("mean_diet_weight", "mean_diet_prop") %in% names(mean_diet_data))) {
    stop("plot_mean_diet_by_strata() requires mean_diet_weight and mean_diet_prop.", call. = FALSE)
  }

  keep_prey <- mean_diet_data |>
    dplyr::group_by(.data[[prey_label_var]]) |>
    dplyr::summarise(score = sum(mean_diet_weight, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(score)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(.data[[prey_label_var]])

  plot_dat <- mean_diet_data |>
    dplyr::filter(.data[[prey_label_var]] %in% keep_prey) |>
    dplyr::group_by(.data[[prey_label_var]], strat) |>
    dplyr::summarise(
      plot_weight = sum(mean_diet_weight, na.rm = TRUE),
      plot_prop = safe_weighted_mean(mean_diet_prop, n_predators),
      .groups = "drop"
    )

  ggplot2::ggplot(plot_dat, ggplot2::aes(x = .data[[prey_label_var]], y = strat)) +
    ggplot2::geom_point(
      ggplot2::aes(size = plot_weight, colour = plot_prop),
      alpha = 0.9
    ) +
    ggplot2::scale_colour_viridis_c(option = "C", limits = c(0, 1), oob = scales::squish) +
    ggplot2::scale_size_continuous(trans = "sqrt", range = c(1.5, 12)) +
    ggplot2::labs(
      title = "Mean Diet by Survey Strata",
      subtitle = "Bubble plot: colour = mean diet proportion, size = mean diet weight",
      x = "Prey",
      y = "Survey stratum",
      colour = "mean_diet_prop",
      size = "mean_diet_weight"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

# Plot unaggregated mean-diet output by length bin as a prey x length bubble plot.
# Color encodes mean_diet_prop, bubble size encodes mean_diet_weight.
plot_mean_diet_by_length <- function(mean_diet_data, top_n = 12, length_bin_var = "length_bin") {
  prey_label_var <- if ("prey_common" %in% names(mean_diet_data)) "prey_common" else "prey_code"
  if (!(length_bin_var %in% names(mean_diet_data))) {
    stop("plot_mean_diet_by_length() requires the specified length_bin_var column.", call. = FALSE)
  }
  if (!all(c("mean_diet_weight", "mean_diet_prop") %in% names(mean_diet_data))) {
    stop("plot_mean_diet_by_length() requires mean_diet_weight and mean_diet_prop.", call. = FALSE)
  }

  keep_prey <- mean_diet_data |>
    dplyr::group_by(.data[[prey_label_var]]) |>
    dplyr::summarise(
      score = sum(mean_diet_weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(score)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(.data[[prey_label_var]])

  plot_dat <- mean_diet_data |>
    dplyr::filter(.data[[prey_label_var]] %in% keep_prey) |>
    dplyr::group_by(.data[[prey_label_var]], .data[[length_bin_var]]) |>
    dplyr::summarise(
      plot_weight = sum(mean_diet_weight, na.rm = TRUE),
      plot_prop = safe_weighted_mean(mean_diet_prop, n_predators),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      length_bin_chr = as.character(.data[[length_bin_var]]),
      length_bin_start = suppressWarnings(as.numeric(sub("^\\s*\\[?\\(?\\s*([-+]?[0-9]*\\.?[0-9]+).*$", "\\1", length_bin_chr)))
    ) |>
    dplyr::arrange(length_bin_start, length_bin_chr) |>
    dplyr::mutate(
      length_bin_ordered = factor(length_bin_chr, levels = unique(length_bin_chr), ordered = TRUE)
    )

  ggplot2::ggplot(plot_dat, ggplot2::aes(x = .data[[prey_label_var]], y = length_bin_ordered)) +
    ggplot2::geom_point(
      ggplot2::aes(size = plot_weight, colour = plot_prop),
      alpha = 0.9
    ) +
    ggplot2::scale_colour_viridis_c(option = "C", limits = c(0, 1), oob = scales::squish) +
    ggplot2::scale_size_continuous(trans = "sqrt", range = c(1.5, 12)) +
    ggplot2::labs(
      title = "Mean Diet by Predator Length Bin",
      subtitle = "Bubble plot: colour = mean diet proportion, size = mean diet weight",
      x = "Prey",
      y = "Length bin",
      colour = "mean_diet_prop",
      size = "mean_diet_weight"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
