# AZMP Scotian Shelf SST from Satellite (ea_data)

Sea surface temperature (SST) from satellite data for the Scotian Shelf,
covering NAFO Divisions 4X, 4V, and 4W. Data are averaged from satellite
observations and are crucial for fisheries and climate analyses.

## Usage

``` r
data(azmp_satellite_temperature)
```

## Format

An `ea_data` object:

- data:

  A tibble with columns:

  - `year`: Numeric year

  - `region`: Character NAFO area name

  - `mean_value`: Numeric sea surface temperature (°C)

- meta:

  A named list, including:

  - `data_type`: "Sea Surface Temperature"

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
