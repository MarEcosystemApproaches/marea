# Arctic Oscillation (AO) Index (ea_data)

The AO Index tracks the primary mode of atmospheric circulation
variability in the Northern Hemisphere, calculated via PCA of 1000 hPa
geopotential anomalies north of 20°N.

## Usage

``` r
data(ao)
```

## Format

An `ea_data` object:

- data:

  Tibble:

  - `year`: Year

  - `month`: Month (1–12)

  - `value`: AO anomaly (unitless index)

- meta:

  Includes:

  - `data_type`: "Arctic Oscillation Index"

  - `region`: "Northern Hemisphere"

  - `location_descriptor`: "AO PCA"

  - `units`: ""

  - `source_citation`: "NOAA CPC"

  - `original_value_col`: "anomaly"

## Source

<https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/>

## Author

Andrew Edwards
