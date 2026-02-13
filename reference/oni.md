# Oceanic Niño Index (ONI)

The Oceanic Niño Index (ONI) is a monthly measure of the El
Niño-Southern Oscillation. It is calculated as a 3-month running mean of
sea surface temperature (SST) anomalies in the Niño 3.4 region (5°N–5°S,
120°W–170°W), using 30-year base periods updated every 5 years. ONI
values may change for up to two months after initial posting due to data
filtering.

## Usage

``` r
data(oni)
```

## Format

An `ea_data` object with:

- data:

  A tibble with columns:

  year

  :   Year (numeric)

  month

  :   Month (numeric, 1–12)

  value

  :   Three-month average sea surface temperature (SST, °C)

  anomaly

  :   SST anomaly (°C)

- meta:

  A named list with:

  data_type

  :   Character. "Oceanic Niño Index (ONI)"

  region

  :   Character. "Pacific"

  location_descriptor

  :   Character. "Niño 3.4 region"

  units

  :   Character. "°C"

  species

  :   Character. `NA_character_` (not applicable)

  source_citation

  :   Character. NOAA CPC, see links above

  original_value_col

  :   Character. Name of value source column, e.g. "val"

## Source

Generated in data-raw/coastwide-indices/coastwide-indices.R

## Details

For more information, see [NOAA
ONI](http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.smd)
and [NOAA ENSO
SST](https://www.ncei.noaa.gov/access/monitoring/enso/sst).

## References

Ross, T. & Robert, M. (2022). In Boldt, J.L. et al. Canadian Technical
Report of Fisheries and Aquatic Sciences, 3482.

## Author

Andrew Edwards
