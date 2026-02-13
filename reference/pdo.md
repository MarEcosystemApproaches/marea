# Pacific Decadal Oscillation (PDO) Index (ea_data)

The PDO Index is a monthly index describing long-term sea surface
temperature variability in the North Pacific (poleward of 20°N).

## Usage

``` r
data(pdo)
```

## Format

An `ea_data` object:

- data:

  Tibble with:

  - `year`: Year

  - `month`: Month (1–12)

  - `value`: PDO anomaly (unitless)

- meta:

  List includes:

  - `data_type`: "Pacific Decadal Oscillation Index"

  - `region`: "North Pacific"

  - `location_descriptor`: "poleward 20°N North Pacific"

  - `units`: ""

  - `source_citation`: "NOAA NCEI; see data-raw"

  - `original_value_col`: "anomaly"

## Source

<https://www.ncei.noaa.gov/access/monitoring/pdo/>

## Author

Andrew Edwards
