# Coastline Data for North Atlantic Region

State and province level boundaries from Natural Earth for Canada,
United States, Greenland, and Saint Pierre and Miquelon. Useful for
adding coastline context to spatial plots of marine data in the
Northwest Atlantic region.

## Usage

``` r
data(coastline)
```

## Format

An `sf` object with 72 features and 4 fields:

- admin:

  Character. Country name (e.g., "Canada", "United States of America")

- name_en:

  Character. State/province name in English

- name_fr:

  Character. State/province name in French

- geometry:

  sfc_MULTIPOLYGON. Spatial geometry of the state/province boundaries

## Source

Natural Earth Data via rnaturalearth package, see
data-raw/coastline/coastline.R

## Details

This dataset includes state/province boundaries for:

- Canada (CA) - All provinces and territories

- United States (US) - States bordering Atlantic coast

- Greenland (GL)

- Saint Pierre and Miquelon (PM)

The data is in WGS 84 coordinate reference system (EPSG:4326) and is
suitable for use with ggplot2 and sf plotting functions.

## References

<https://www.naturalearthdata.com/>

## Author

marea/DFO
