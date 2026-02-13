# Satellite Ocean Colour (ea_spatial)

Annual spring bloom metrics and average chlorophyll-a derived from the
OC-CCI POLY4 chl-a product in the Northwest Atlantic. Pulled using
marea::get_erddap_data(). NRMSE_bloom \> 0.5 and \|standardized
anomalies\| \> 4 filtered out. Resampled from 4.64 km to 0.1 degree
resolution.

## Usage

``` r
data(satellite_ocean_colour)
```

## Format

An `ea_spatial` (stars) object:

- t_start_value:

  Day of year of the start of the spring phytoplankton bloom (day of
  year)

- t_duration_value:

  Duration of the spring phytoplankton bloom (days)

- amplitude_real_value:

  Maximum concentration during the spring phytoplankton bloom period
  (mg/m3)

- magnitude_real_value:

  Total chlorophyll-a produced during the spring phytoplankton bloom
  period (days x mg/m3)

- annual_mean_value:

  Average chlorophyll-a over the year (mg/m3)

**Metadata (`attr(obj, "meta")`)**:

- data_type:

  Character. "t_start: Day of year of the start of the spring
  phytoplankton bloom; t_duration: Duration of the spring phytoplankton
  bloom; amplitude_real: Maximum concentration during the spring
  phytoplankton bloom period; magnitude_real: Total chlorophyll-a
  produced during the spring phytoplankton bloom period; annual_mean:
  Average chlorophyll-a over the year"

- units:

  Character. "t_start: day of year; t_duration: days; amplitude_real:
  mg/m3; magnitude_real: days\*mg/m3; annual_mean: mg/m3"

- region:

  Character. "Northwest Atlantic"

- time_descriptor:

  Character. Year

- source_citation:

  Character. "Clay, S., & Devred, E. (2025). Northwest Atlantic
  per-pixel spring bloom metrics derived from OC-CCI POLY4 satellite
  Chlorophyll-a (v1.1) \[Data set\].
  https://catalogue.cioosatlantic.ca/dataset/ca-cioos_473fe40d-613e-4aba-b335-7a31a04e5d69?local=en"

- temporal_coverage:

  Character. "1998-2024"

- variable_name:

  Character. The name used for the variable

## Source

SOPhyE group, DFO, see
data-raw/satellite-ocean-colour/satellite_ocean_colour.R

## Author

marea/DFO
