# Download Copernicus Marine (CMEMS) data

Downloads data from the Copernicus Marine Environment Monitoring Service
([CMEMS](https://marine.copernicus.eu/)) using the Copernicus Marine API
and the Python package `copernicusmarine`.

## Usage

``` r
get_CMEMS_ncdf(
  username = NA,
  password = NA,
  dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
  variables,
  minimum_longitude = -67.7425,
  maximum_longitude = -54.90132,
  minimum_latitude = 40.04343,
  maximum_latitude = 47.83333,
  start_datetime = "1993-12-01T00:00:00",
  end_datetime = "1994-12-01T00:00:00",
  output_filename = tempfile(fileext = ".nc")
)
```

## Arguments

- username:

  Your CMEMS username.

- password:

  Your CMEMS password.

- dataset_id:

  Dataset ID (default is "cmems_mod_glo_phy_my_0.083deg_P1M-m", the
  global ocean physics product).

- variables:

  list of variable names to download.

- minimum_longitude:

  Minimum longitude for the data bounding box.

- maximum_longitude:

  Maximum longitude for the data bounding box.

- minimum_latitude:

  Minimum latitude for the data bounding box.

- maximum_latitude:

  Maximum latitude for the data bounding box.

- start_datetime:

  Start date and time (e.g., "1993-12-01T00:00:00").

- end_datetime:

  End date and time (e.g., "1994-12-01T00:00:00").

- output_filename:

  Name of the output file (NetCDF format).

## Value

No return value. Downloads a NetCDF file to the specified location.

## Examples

``` r
if (FALSE) { # \dontrun{
get_CMEMS_ncdf(username = "your_username", password = "your_password", variables = list("thetao"))
} # }
```
