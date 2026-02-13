# AZMP Scotian Shelf Surface Temperature (ea_data)

Surface water temperature (annual mean) from the Atlantic Zone
Monitoring Program (AZMP) for the Scotian Shelf, covering NAFO Divisions
4X, 4V, and 4W. Data are averaged from in situ surveys and are crucial
for fisheries and climate analyses.

## Usage

``` r
data(azmp_surface_temperature)
```

## Format

An `ea_data` object:

- data:

  A tibble with columns:

  - `year`: Numeric year

  - `region`: Character area name

  - `mean_value`: Numeric temperature at sea surface (°C)

- meta:

  A named list, including:

  - `data_type`: "Surface Temperature"

  - `region`: "Scotian Shelf"

  - `location_descriptor`: "NAFO 4X-4W"

  - `units`: "°C"

  - `species`: `NA`

  - `source_citation`: "DFO AZMP, through azmpdata package
    https://github.com/casaultb/azmpdata/"

## Source

Generated from running `data-raw/azmpdata/azmpdata-load.R`

## Author

Emily O'Grady and Benoit Casault
