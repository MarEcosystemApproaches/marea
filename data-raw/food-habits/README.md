# Food Habits Documentation

This folder contains the food-habits processing workflow used to generate data products related to the food habits database for use in the MAREA package.

This README file document provides details on the processing steps and output formats.

For more specific documentation, refer to in-line comments of the processing script (`food-habits.R`) and to the roxygen-style documentation of the methods implemented (`estimate_mean_diet`, `estimate_dominant_prey`, `estimate_predator_contribution`).

## Recommended dataset to use for exploration

Use `food_habits_stomach` as the complete stomach/prey table.


## 1) Code documentation

Documentation is available in:

- `food-habits.R` documents the end-to-end workflow and output tables.
- `food-habits-estimate-mean-diet.R` has detailed function documentation (method steps, assumptions, parameters, return columns).
- `food-habits-estimate-dominant-prey.R` and `food-habits-estimate-predator-contribution.R` document filters, calculations, and outputs.

Refer to these files for detailed documentation on methods, their implementation and their outputs.

## 2) Processing overview

Main script: `data-raw/food-habits/food-habits.R`

Pipeline steps:

1. Load source `.RData` inputs (`GROUNDFISH_STOMACH_DATA_VW.RData`, `GROUNDFISH_GSSPECIES.RData`).
2. Standardize and join species labels (predator + prey).
3. Filter to configured priority predators and optional grouped codes.
4. Build three analytical outputs:
   - stratified mean diet,
   - dominant prey time series,
   - predator contribution on focal prey.
5. Save `.rda` outputs in this folder and export package data with `usethis::use_data(...)`.

## 3) Key configuration defaults (from `food-habits.R`)

- Priority predators (`priority_predator_codes`): 10, 11, 16, 14, 20, 40.
- Priority prey (`priority_prey_codes`): 60, 2211, 6411, 6400.
- Mean-diet grouping: `c("year", "strat", "pred_code")`.
- Dominant-prey grouping: `c("year", "pred_code")`.
- Predator-contribution grouping: `c("year", "nafo_zone", "prey_code")`.
- Length breaks: `seq(0, 150, by = 5)`.
- Exclusion list applied by default: `remove_excluded_codes = TRUE`.
- Mean-diet denominator mode default: `"usable_predators"` (see note below).

## 4) Output files and schemas

### `food_habits_stomach.rda`

Full, processed food habits dataset. 

Row: individual prey record within a predator stomach/sample context.

Columns:
- `set_seq`, `pred_seq`, `prey_seq`: sequence IDs from source.
- `datasource`, `mission`, `setno`: survey/set identifiers.
- `sdate`, `year`, `month`: sample date and derived year/month.
- `strat`, `nafo_zone`, `nafo_subunit`: spatial strata/NAFO fields.
- `slatdd`, `slongdd`, `depth`, `bottom_temperature`: location/environment fields.
- `status_flag`, `gear`, `fshno`, `tech`: sampling metadata.
- `fwt`, `flen`, `fgen`, `fwt_calculated`: predator attributes.
- `stowgt`, `emptywgt`, `stomach_content_wt`: stomach weights (`stomach_content_wt = stowgt - emptywgt` when both available).
- `fullness`: stomach fullness code.
- `pwt`, `plen`, `pnum`, `rank`, `digestion`, `remarks`, `preyvalue`: prey record fields.
- `pred_code`, `pred_common`, `pred_latin`: predator species code and labels.
- `prey_code`, `prey_common`, `prey_latin`: prey species code and labels.
- `has_prey_record`: logical flag (`!is.na(prey_code)`).

### `food_habits_species.rda`

Species lookup table used to label predator/prey codes.

Row: one species code.

Columns:
- `species_code`: integer species code.
- `common_name`: common name.
- `latin_name`: latin name.

### `food_habits_mean_diet_stratified.rda`

Output of `estimate_mean_diet()` using the configured grouping and length/strata logic.

Row: one prey taxon within each (`year`, `strat`, predator) group.

Columns:
- `year`, `strat`: reporting groups.
- `pred_code`, `pred_common`: predator code and label.
- `prey_code`, `prey_common`: prey code and label.
- `mean_diet_weight`: estimated mean prey weight per predator, aggregated with length and/or strata weighting.
- `mean_diet_prop`: estimated mean prey proportion (`prey_weight / predator_total_weight`) averaged with same weighting logic.
- `mean_occurrence_prop`: estimated occurrence proportion (share of predators with prey present) under same weighting logic.
- `n_predators`: predators in denominator after aggregation.
- `n_strata`: number of strata contributing to each row.
- `prey_rank`: dense rank within group by descending `mean_diet_weight`.

### `food_habits_dominant_prey_timeseries.rda`

Output of `estimate_dominant_prey()`, which calls `estimate_mean_diet()` then filters dominant prey.

Row: one retained prey taxon within each (`year`, predator) group.

Columns:
- `year`.
- `pred_code`, `pred_common`.
- `prey_code`, `prey_common`.
- `mean_diet_weight`, `mean_diet_prop`, `mean_occurrence_prop`, `n_predators`, `n_strata`, `prey_rank` (same definitions as mean diet).

Dominance filters (configurable):
- minimum `mean_diet_prop`,
- minimum `mean_occurrence_prop`,
- top-N by `prey_rank`.

### `food_habits_prey_predation.rda`

Output of `estimate_predator_contribution()`.

Row: one predator within each prey-centric group (`year`, `nafo_zone`, `prey_code` by default).

Columns:
- `year`, `nafo_zone`.
- `prey_code`, `prey_common`.
- `pred_code`, `pred_common`.
- `prey_weight_total`: total prey weight attributed to that predator in the group (`sum(pwt)`).
- `n_prey_records`: number of prey rows contributing.
- `n_stomachs`: distinct stomach count (`n_distinct(pred_seq)` by default).
- `predator_weight_prop`: predator share of total prey weight in group (`prey_weight_total / sum(prey_weight_total)` within group).
- `predator_rank`: dense rank within group by descending `prey_weight_total`.

Optional filters:
- keep only top-N predators by rank,
- keep only predators above minimum contribution threshold.

## 5) Note on `denominator mode`

We add a note on the `denominator_mode` argument in `estimate_mean_diet()`, which also affects `estimate_dominant_prey()` by extension. 

Options:

- `"usable_predators"` (default): denominator includes predators with at least one usable prey row after filtering. This means that predators without usable prey data (prey removed or stomach empty) are removed from the calculations. 
- `"all_sampled_predators"`: denominator includes all sampled predators with valid predator ID/stratum/length-bin fields, including predators without usable prey rows.

Why this matters:

- It changes `n_predators`, and therefore changes `mean_diet_weight`, `mean_diet_prop`, and `mean_occurrence_prop`.
- In datasets with many empty or unusable stomach records, `"all_sampled_predators"` usually produces lower prey means/proportions/occurrence than `"usable_predators"` because the denominator is larger.
- `"usable_predators"` is a conditional estimate (among predators with usable prey information), while `"all_sampled_predators"` is closer to a whole-sampled-predator estimate.

The `mean_diet_denominator_mode` configuration object controls this option globally in the `food_habits.R` processing script.

***Careful consideration should be given to this before generating the outputs.***

## 6) Reproducibility

To regenerate outputs, run:

```r
source("data-raw/food-habits/food-habits.R")
```

The script will regenerate `.rda` files in `data-raw/food-habits/` and refresh package `data/` objects via `usethis::use_data(...)`.
