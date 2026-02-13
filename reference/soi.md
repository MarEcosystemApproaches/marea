# Southern Oscillation Index (SOI) (ea_data)

The SOI is a monthly measure of the seasonal atmospheric pressure
differences between Tahiti and Darwin, a leading indicator of El Niño/La
Niña conditions.

## Usage

``` r
data(soi)
```

## Format

An `ea_data` object:

- data:

  Tibble with:

  - `year`: Year

  - `month`: Month (1–12)

  - `value`: SOI value (unitless index)

- meta:

  Metadata includes

  - `data_type`: "Southern Oscillation Index"

  - `region`: "South Pacific"

  - `location_descriptor`: "Tahiti-Darwin"

  - `units`: "index"

  - `source_citation`: "NOAA CPC"

  - `original_value_col`: "anomaly"

## Source

<https://www.cpc.ncep.noaa.gov/data/indices/soi>

## Author

Andrew Edwards
