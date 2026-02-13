# AZMP Scotian Shelf Stratification Index (ea_data)

Stratification index ( measured as the density difference between 0 and
50 metres) from the Atlantic Zone Monitoring Program (AZMP) for the
Scotian Shelf, covering NAFO Divisions 4X, 4V, and 4W. Data are averaged
from in situ surveys and are crucial for fisheries and climate analyses.

## Usage

``` r
data(azmp_stratification)
```

## Format

An `ea_data` object:

- data:

  A tibble with columns:

  - `year`: Numeric year

  - `region`: Character NAFO area name

  - `mean_value`: Numeric stratification index (kg/m^3)

- meta:

  A named list, including:

  - `data_type`: "Stratification Index"

  - `region`: "Scotian Shelf"

  - `location_descriptor`: "NAFO 4X-4W"

  - `units`: "kg/m^3"

  - `species`: `NA`

  - `source_citation`: "DFO AZMP, through azmpdata package
    https://github.com/casaultb/azmpdata/"

## Source

Generated from running `data-raw/azmpdata/azmpdata-load.R`

## Author

Emily O'Grady and Benoit Casault
