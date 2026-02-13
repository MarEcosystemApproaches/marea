# GLORYS Bottom Temperature (ea_spatial)

Monthly bottom temperature fields from the GLORYS reanalysis, provided
as a spatial object for the Maritimes region. The latest ten years of
data are included from the Copernicus Marine Service (CMEMS) GLORYS
product (monthly and interim monthly) for best data coverage.

## Usage

``` r
data(glorys_bottom_temperature)
```

## Format

An `ea_spatial` (sf) object:

- value:

  Bottom temperature value (°C)

- month:

  Month of observation

- geometry:

  Spatial geometry (sf object)

**Metadata (`attr(obj, "meta")`)**:

- data_type:

  Character. "GLORYS Bottom Temperature"

- region:

  Character. "Maritimes"

- time_descriptor:

  Character. Month, year, or period

- units:

  Character. "°C"

- source_citation:

  Character. "Copernicus Marine Service"

- original_value_col:

  Character. The original col used for value

## Source

Copernicus Marine Service (GLORYS), see
data-raw/glorys/glorys-bottom-temperature.R

## Details

Dataset IDs: cmems_mod_glo_phy_my_0.083deg_P1M-m and
cmems_mod_glo_phy_myint_0.083deg_P1M-m Pulled using
marea::get_CMEMS_ncdf()

## Author

marea/DFO
