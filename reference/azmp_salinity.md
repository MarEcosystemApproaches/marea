# AZMP Surface Salinity (ea_data)

Surface salinity from the Atlantic Zone Monitoring Program (AZMP) for
the Scotian Shelf, covering NAFO Divisions 4X, 4V, and 4W. Data are
averaged from in situ surveys and are crucial for fisheries and climate
analyses.

## Usage

``` r
data(azmp_salinity)
```

## Format

An `ea_data` object:

- data:

  A tibble with columns:

  - `year`: Numeric year

  - `region`: Character NAFO area name

  - `mean_value`: Numeric surface salinity (psu)

- meta:

  A named list, including:

  - `data_type`: "Surface Salinity"

  - `region`: "Scotian Shelf"

  - `location_descriptor`: "NAFO 4X-4W"

  - `units`: "psu"

  - `species`: `NA`

  - `source_citation`: "DFO AZMP, through azmpdata package
    https://github.com/casaultb/azmpdata/"

## Source

Generated from running `data-raw/azmpdata/azmpdata-load.R`

## Author

Emily O'Grady and Benoit Casault
