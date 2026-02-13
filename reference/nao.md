# North Atlantic Oscillation (NAO) Index (ea_data)

The NAO Index represents the difference in sea-level atmospheric
pressures between the Azores and Iceland, calculated by PCA of 500 mb
height anomalies.

## Usage

``` r
data(nao)
```

## Format

An `ea_data` object:

- data:

  A tibble:

  year

  :   Year

  value

  :   NAO anomaly value

- meta:

  A named list. Keys include:

  - `data_type`: "NAO Index"

  - `region`: "North Atlantic"

  - `location_descriptor`: "Azores-Iceland"

  - `units`: "index"

  - `species`: `NA`

  - `source_citation`: "NOAA, see source"

  - `original_value_col`: e.g. "anomaly"

## Source

<https://www.ncei.noaa.gov/access/monitoring/nao/>

## Author

Jamie C. Tam; Chantelle Layton
