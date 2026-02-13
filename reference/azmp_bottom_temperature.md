# AZMP Scotian Shelf Bottom Temperature (ea_data)

Bottom water temperature (annual mean) from the Atlantic Zone Monitoring
Program (AZMP) for the Scotian Shelf, covering NAFO Divisions 4X, 4V,
and 4W. Data are averaged from in situ surveys and are crucial for
fisheries and climate analyses.

## Usage

``` r
data(azmp_bottom_temperature)
```

## Format

An `ea_data` object:

- data:

  A tibble with columns:

  - `year`: Numeric year

  - `region`: Character NAFO division or area name

  - `value`: Numeric temperature at sea floor (°C)

- meta:

  A named list, including:

  - `data_type`: "Annual Bottom Temperature"

  - `region`: "Scotian Shelf"

  - `location_descriptor`: "NAFO 4X-4W"

  - `units`: "°C"

  - `species`: `NA`

  - `source_citation`: "DFO AZMP,
    https://www.bio.gc.ca/science/monitoring-monitorage/azmp-pmza/overview-eng.php"

  - `original_value_col`: "value"

## Source

Generated from running `data-raw/azmpdata/azmpdata-load.R`

## Author

Jaimie Harbin and Benoit Casault
