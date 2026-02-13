# North Pacific Gyre Oscillation (NPGO) Index (ea_data)

The NPGO is the second dominant mode of sea-surface height anomaly
variability in the northeast Pacific, correlating with
nutrient/chlorophyll fluctuations.

## Usage

``` r
data(npgo)
```

## Format

An `ea_data` object:

- data:

  Tibble:

  - `year`: Year

  - `month`: Month

  - `value`: NPGO anomaly (index, unitless)

- meta:

  Includes:

  - `data_type`: "North Pacific Gyre Oscillation Index"

  - `region`: "North Pacific"

  - `location_descriptor`: "NPGO region"

  - `units`: ""

  - `source_citation`: "UCSD/SIO; see data-raw"

  - `original_value_col`: "anomaly"

## Source

<http://www.o3d.org/npgo/>

## Author

Andrew Edwards
