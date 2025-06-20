
<!--
README.md is generated from README.Rmd. Please edit that file. Build with:
-->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/MarEcosystemApproaches/marea/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MarEcosystemApproaches/marea/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/MarEcosystemApproaches/marea/graph/badge.svg?token=93afkFJUVL)](https://codecov.io/gh/MarEcosystemApproaches/marea)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15706086.svg)](https://doi.org/10.5281/zenodo.15706086)
<!-- badges: end -->

# marea <img src="man/figures/marea_virdis_logo.png" align="right" height="138" /></a>

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

If you use `marea` in your work, please cite:

“Edwards, A.M. et al. (2024). marea: Maritime Ecosystem Approach R
Package. R package version 1.0.0.
<https://doi.org/10.5281/zenodo.15706086>”

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
