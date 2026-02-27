#' Dominant Prey Time Series from Stratified Mean Diet Estimates
#'
#' Generate time-series-ready dominant prey summaries by applying dominance
#' filters to outputs from `estimate_mean_diet()`.
#'
#' @description
#' `estimate_dominant_prey()` computes stratified diet estimates and
#' then retains dominant prey using configurable selection rules:
#' top-N rank, minimum diet proportion, and/or minimum occurrence proportion.
#'
#' @details
#' The function first calls `estimate_mean_diet()` using the supplied
#' grouping and weighting arguments. Dominance filtering is then applied in this
#' order:
#' 1. `min_diet_prop` filter (if provided),
#' 2. `min_occurrence_prop` filter (if provided),
#' 3. `top_n` rank filter (if provided).
#'
#' This design keeps weighted diet estimation centralized while making
#' dominant-prey extraction explicit and configurable for
#' management reporting workflows.
#'
#' @param food_habits_stomach Standardized stomach-prey data table.
#' @param group_vars Character vector of grouping variables used to define each
#' time-series unit. Typical default: `c("year", "pred_code")`.
#' @param top_n Optional integer; keep prey with rank `<= top_n`.
#' @param min_diet_prop Optional numeric threshold on `mean_diet_prop`.
#' @param min_occurrence_prop Optional numeric threshold on
#' `mean_occurrence_prop`.
#' @param include_label_cols Logical; if `TRUE`, include mapped readable labels
#' (for example `pred_common`, `prey_common`) when code columns are grouped.
#' @param ... Additional arguments passed directly to
#' `estimate_mean_diet()` (for example weighting tables, length bin
#' controls, strata columns).
#'
#' @return A tibble of dominant prey time-series records with the fields returned
#' by `estimate_mean_diet()` after filtering, ordered by grouping
#' fields and prey rank.
#'
#' @examples
#' # Example call pattern:
#' # estimate_dominant_prey(
#' #   food_habits_stomach = food_habits_stomach,
#' #   group_vars = c("year", "pred_code"),
#' #   top_n = 5
#' # )
#'
#' @export
estimate_dominant_prey <- function(
    food_habits_stomach,
    group_vars = c("year", "pred_code"),
    top_n = 5,
    min_diet_prop = NULL,
    min_occurrence_prop = NULL,
    include_label_cols = TRUE,
    ...) {

  diet <- estimate_mean_diet(
    food_habits_stomach = food_habits_stomach,
    group_vars = group_vars,
    include_label_cols = include_label_cols,
    ...
  )

  if (!is.null(min_diet_prop)) {
    diet <- diet %>% dplyr::filter(mean_diet_prop >= min_diet_prop)
  }

  if (!is.null(min_occurrence_prop)) {
    diet <- diet %>% dplyr::filter(mean_occurrence_prop >= min_occurrence_prop)
  }

  if (!is.null(top_n)) {
    diet <- diet %>% dplyr::filter(prey_rank <= top_n)
  }

  diet %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(existing_cols(diet, group_vars))), prey_rank)
}

# Plot summary for dominant prey estimates.
# Uses stacked bars by year for readability in multi-prey time series.
plot_dominant_prey <- function(dominant_prey_data, top_n = 8, facet_by_predator = TRUE) {
  prey_label_var <- if ("prey_common" %in% names(dominant_prey_data)) "prey_common" else "prey_code"
  pred_label_var <- if ("pred_common" %in% names(dominant_prey_data)) "pred_common" else if ("pred_code" %in% names(dominant_prey_data)) "pred_code" else NULL

  keep_prey <- dominant_prey_data %>%
    dplyr::group_by(.data[[prey_label_var]]) %>%
    dplyr::summarise(score = sum(mean_diet_prop, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(.data[[prey_label_var]])

  plot_dat <- dominant_prey_data %>%
    dplyr::filter(.data[[prey_label_var]] %in% keep_prey)

  if ("year" %in% names(plot_dat)) {
    group_cols <- c("year", prey_label_var)
    if (!is.null(pred_label_var)) {
      group_cols <- c(group_cols, pred_label_var)
    }

    ts_dat <- plot_dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(plot_value = sum(mean_diet_prop, na.rm = TRUE), .groups = "drop")

    p <- ggplot2::ggplot(
      ts_dat,
      ggplot2::aes(x = factor(year), y = plot_value, fill = .data[[prey_label_var]])
    ) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::labs(
        title = "Dominant Prey Over Time",
        subtitle = "Stacked prey composition by year (top prey only)",
        x = "Year",
        y = "Summed estimated mean diet proportion",
        fill = "Prey"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    if (isTRUE(facet_by_predator) && !is.null(pred_label_var) && dplyr::n_distinct(ts_dat[[pred_label_var]]) > 1) {
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", pred_label_var)), scales = "free_y")
    }

    p
  } else {
    bar_dat <- plot_dat %>%
      dplyr::group_by(.data[[prey_label_var]]) %>%
      dplyr::summarise(value = sum(mean_diet_prop, na.rm = TRUE), .groups = "drop")

    ggplot2::ggplot(bar_dat, ggplot2::aes(x = stats::reorder(.data[[prey_label_var]], value), y = value)) +
      ggplot2::geom_col(fill = "#4B9CD3") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Dominant Prey",
        subtitle = "Aggregated across available groups (no year field present)",
        x = "Prey",
        y = "Aggregated mean diet proportion"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  }
}
