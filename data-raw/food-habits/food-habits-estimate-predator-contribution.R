#' Predation on Prey Species by Predator Contribution
#'
#' Estimate predation patterns on focal prey by summarizing predator
#' contributions (consumed weight and relative share).
#'
#' @description
#' `estimate_predator_contribution()` provides prey summaries of predation by
#' predator. It first computes predator contribution metrics using
#' `summarise_prey_pressure()`, then optionally filters results to dominant
#' predators by contribution thresholds or top-N rank.
#'
#' @details
#' Workflow:
#' 1. Aggregate consumed prey weight by grouping variables and predator.
#' 2. Compute predator contribution proportion within each grouping unit.
#' 3. Rank predators by descending prey weight contribution.
#' 4. Optionally filter by:
#'   - minimum contribution proportion (`min_predator_contribution`)
#'   - top-N predator rank (`top_n_predators`)
#'
#' @param food_habits_stomach Standardized stomach-prey data table.
#' @param group_vars Character vector of grouping variables. Typical default:
#' `c("year", "nafo_zone", "prey_code")`.
#' @param prey_var Character scalar naming prey code column.
#' @param predator_var Character scalar naming predator code column.
#' @param weight_var Character scalar naming prey-weight column.
#' @param stomach_id_var Character scalar naming predator/stomach ID.
#' @param remove_excluded_codes Logical. If `TRUE`, remove excluded prey codes
#' before estimation.
#' @param excluded_prey_codes Integer vector of prey codes to exclude when
#' `remove_excluded_codes = TRUE`.
#' @param include_label_cols Logical; if `TRUE`, include mapped readable labels
#' (for example `pred_common`, `prey_common`) when code fields are grouped.
#' @param label_map Named character vector mapping code columns to label columns.
#' @param top_n_predators Optional integer; keep predator rows with rank
#' `<= top_n_predators`.
#' @param min_predator_contribution Optional numeric threshold on
#' `predator_weight_prop`.
#'
#' @return A tibble with predator contribution summaries per prey grouping,
#' including:
#' \describe{
#'   \item{prey_weight_total}{Total prey weight attributed to each predator}
#'   \item{n_prey_records}{Number of prey records contributing}
#'   \item{n_stomachs}{Number of distinct stomachs contributing}
#'   \item{predator_weight_prop}{Predator contribution proportion within group}
#'   \item{predator_rank}{Within-group predator rank by prey weight}
#' }
#'
#' @examples
#' # Example call pattern:
#' # estimate_predator_contribution(
#' #   food_habits_stomach = food_habits_stomach,
#' #   group_vars = c("year", "nafo_zone", "prey_code"),
#' #   top_n_predators = 5
#' # )
#'
#' @export
estimate_predator_contribution <- function(
  food_habits_stomach,
  group_vars = c("year", "nafo_zone", "prey_code"),
  prey_var = "prey_code",
  predator_var = "pred_code",
  weight_var = "pwt",
  stomach_id_var = "pred_seq",
  remove_excluded_codes = TRUE,
  excluded_prey_codes = food_habits_default_exclusion_prey_codes(),
  include_label_cols = TRUE,
  label_map = food_habits_default_label_map(),
  top_n_predators = NULL,
  min_predator_contribution = NULL
) {
  out <- summarise_prey_pressure(
    food_habits_stomach = food_habits_stomach,
    group_vars = group_vars,
    prey_var = prey_var,
    predator_var = predator_var,
    weight_var = weight_var,
    stomach_id_var = stomach_id_var,
    remove_excluded_codes = remove_excluded_codes,
    excluded_prey_codes = excluded_prey_codes,
    include_label_cols = include_label_cols,
    label_map = label_map
  )

  if (!is.null(min_predator_contribution)) {
    out <- out |> dplyr::filter(predator_weight_prop >= min_predator_contribution)
  }

  if (!is.null(top_n_predators)) {
    out <- out |> dplyr::filter(predator_rank <= top_n_predators)
  }

  out
}


# Internal helper: summarize predator contributions to each prey
# group and compute within-group predator ranks.
summarise_prey_pressure <- function(
  food_habits_stomach,
  group_vars = c("year", "nafo_zone", "prey_code"),
  prey_var = "prey_code",
  predator_var = "pred_code",
  weight_var = "pwt",
  stomach_id_var = "pred_seq",
  remove_excluded_codes = TRUE,
  excluded_prey_codes = food_habits_default_exclusion_prey_codes(),
  include_label_cols = TRUE,
  label_map = food_habits_default_label_map()
) {
  group_vars <- existing_cols(food_habits_stomach, group_vars)
  prey_var <- existing_cols(food_habits_stomach, prey_var)
  predator_var <- existing_cols(food_habits_stomach, predator_var)
  if (length(prey_var) != 1) {
    stop("prey_var must resolve to one existing column.", call. = FALSE)
  }
  if (length(predator_var) != 1) {
    stop("predator_var must resolve to one existing column.", call. = FALSE)
  }

  food_habits_stomach <- apply_prey_code_exclusions(
    food_habits_stomach = food_habits_stomach,
    prey_var = prey_var,
    remove_excluded_codes = remove_excluded_codes,
    excluded_prey_codes = excluded_prey_codes
  )

  if (include_label_cols) {
    group_vars <- add_label_cols(food_habits_stomach, group_vars, label_map = label_map)
    predator_var_with_labels <- add_label_cols(food_habits_stomach, predator_var, label_map = label_map)
  } else {
    predator_var_with_labels <- predator_var
  }

  full_group <- unique(c(group_vars, predator_var_with_labels))

  out <- food_habits_stomach |>
    dplyr::filter(!is.na(.data[[predator_var]]), !is.na(.data[[weight_var]]), .data[[weight_var]] >= 0) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(full_group))) |>
    dplyr::summarise(
      prey_weight_total = sum(.data[[weight_var]], na.rm = TRUE),
      n_prey_records = dplyr::n(),
      n_stomachs = dplyr::n_distinct(.data[[stomach_id_var]]),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate(
      predator_weight_prop = prey_weight_total / sum(prey_weight_total, na.rm = TRUE),
      predator_rank = dplyr::dense_rank(dplyr::desc(prey_weight_total))
    ) |>
    dplyr::ungroup()

  out
}

# Plot summary for predator contribution estimates.
plot_predator_contribution <- function(predator_contribution_data, top_n = 8, facet_by_prey = TRUE) {
  pred_label_var <- if ("pred_common" %in% names(predator_contribution_data)) {
    "pred_common"
  } else {
    "pred_code"
  }
  prey_label_var <- if ("prey_common" %in% names(predator_contribution_data)) {
    "prey_common"
  } else {
    "prey_code"
  }

  plot_dat <- predator_contribution_data |>
    dplyr::filter(
      !is.na(.data[[pred_label_var]]),
      !is.na(.data[[prey_label_var]]),
      !is.na(prey_weight_total)
    ) |>
    dplyr::group_by(.data[[prey_label_var]], .data[[pred_label_var]]) |>
    dplyr::summarise(plot_value = sum(prey_weight_total, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::slice_max(order_by = plot_value, n = top_n, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      predator_f = stats::reorder(.data[[pred_label_var]], plot_value)
    )

  p <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = predator_f, y = plot_value)) +
    ggplot2::geom_col(fill = "#3F8A5F") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Predator Contribution by Focal Prey",
      subtitle = "Top predator contributors per prey species by consumed weight",
      x = "Predator",
      y = "Aggregated prey weight total"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (isTRUE(facet_by_prey) && dplyr::n_distinct(plot_dat[[prey_label_var]]) > 1) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", prey_label_var)), scales = "free_y")
  }

  p
}
