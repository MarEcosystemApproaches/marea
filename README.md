
<!--
README.md is generated from README.Rmd. Please edit that file. Build with:
-->
<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-under%20development-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#under%20development)
[![R-CMD-check](https://github.com/MarEcosystemApproaches/marea/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MarEcosystemApproaches/marea/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/MarEcosystemApproaches/marea/graph/badge.svg?token=93afkFJUVL)](https://codecov.io/gh/MarEcosystemApproaches/marea)
<!-- badges: end -->

**UNDER DEVELOPMENT**

# Overview

The marea package provides curated maritime ecosystem data and model
outputs to support ecosystem approaches to fisheries management in
Atlantic Canada. The package standardizes access to environmental
variables, oceanographic indices, and biological data from multiple
sources.

# Installation

``` r
# Install development version from GitHub
install.packages("remotes")
remotes::install_github("MarEcosystemApproaches/marea")
```

For DFO network users experiencing timeout errors:

``` r
options(timeout = 1200)
remotes::install_github("MarEcosystemApproaches/marea")
```

# Quick Start

``` r
library(marea)

# Plot bottom temperature patterns
plot(glorys_bottom_temperature,
     months = c("February", "October"),
     years = c(2015, 2020))

# Access North Atlantic Oscillation index
plot(nao, lwd = 2)

# View grey seal population trends
plot(grey_seals)
```

# Data Products Available

The marea package includes a variety of datasets and model outputs,
including:

| Dataset                   | Temporal.Coverage | Source  | Update.Frequency |
|:--------------------------|:------------------|:--------|:-----------------|
| azmp_bottom_temperature   | 1950-2022         | Unknown | Unknown          |
| food_habits               | Inf–Inf           | Unknown | Unknown          |
| glorys_bottom_temperature | Inf–Inf           | Unknown | Unknown          |
| grey_seals                | 1960-2021         | Unknown | Unknown          |
| grey_seals_2021           | 1960-2021         | Unknown | Unknown          |
| nao                       | 1951-2024         | Unknown | Unknown          |
| oni                       | 1950-2025         | Unknown | Unknown          |

# Documentation

Detailed vignettes with examples and methodology:

*links to vignettes*

# Example Visualizations

## Bottom Temperature Comparison

<img src="man/figures/README-glorys-temp.png" width="70%" />

## Grey Seal Population Trends

<img src="man/figures/README-grey-seals.png" width="70%" />

# Citation

If you use marea in your work, please cite:

“Maritime Ecosystem Approaches (2024). marea: Maritime Ecosystem
Approach R Package. Version 0.1.0.
<https://github.com/MarEcosystemApproaches/marea>”

Use `citation("marea")` for BibTeX format.

# Related Work

This package is part of a national effort to facilitate ecosystem
approaches to fisheries management:

[pacea](https://github.com/pbs-assess/PACea/) - Pacific ecosystem data

[gslea](https://github.com/duplisea/gslea/) - Gulf of St. Lawrence
ecosystem data

# Contributing

Please see our contribution guidelines for information on reporting
issues, suggesting enhancements, and contributing code.

# Acknowledgments

We acknowledge funding from Fisheries and Oceans Canada. We thank the
data providers and contributors who have made this package possible.

# Contact

For questions or support, please open an issue on GitHub.
