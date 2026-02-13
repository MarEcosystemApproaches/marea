# Estimated Grey Seal Abundance (ea_data)

Abundance of grey seals in Canadian waters, assessed as a single stock.
Derived from aerial surveys, corrected for survey timing, coverage, and
haul-out proportion.

## Usage

``` r
data(grey_seals)

data(grey_seals_2021)
```

## Format

An `ea_data` object with:

- data:

  A tibble:

  year

  :   Year (numeric)

  value

  :   Median estimate (count)

  low

  :   Lower 95percent credible interval

  high

  :   Upper 95percent credible interval

- meta:

  A named list with:

  data_type

  :   "Grey Seal Abundance"

  region

  :   "Scotian Shelf"

  location_descriptor

  :   "Sable Island / Scotian Shelf"

  units

  :   "count"

  species

  :   "Halichoerus grypus"

  source_citation

  :   Hammill et al. (2023)

  original_value_col

  :   Character. "median"

An object of class `ea_data` of length 1.

## Source

Hammill et al. (2023), see data-raw/grey-seals/grey-seals.R

## Author

Andrew Edwards, Nell den Heyer
