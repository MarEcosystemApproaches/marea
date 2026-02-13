# An S4 Class for Spatio-Temporal Environmental Assessment Data

The `ea_spatial` class is a container for environmental assessment data
that has a spatial component. It is designed to be flexible, supporting
vector data via `sf` objects and raster data via `stars` or
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
objects.

It standardizes the data by encapsulating a spatial object in the `data`
slot and a list of descriptive information in the `meta` slot.

## Slots

- `meta`:

  A named list containing metadata. Expected fields include `data_type`,
  `region`, `time_descriptor`, `units`, `source_citation`, and
  `original_value_col`.

- `data`:

  A spatial object. Must be of class `sf`, `stars`, or `SpatRaster`. The
  object is expected to have a column or layer named `value` containing
  the primary data.

## See also

[`ea_spatial`](https://marecosystemapproaches.github.io/marea/reference/ea_spatial.md)
for the constructor function.

[`ea.subset.spatial`](https://marecosystemapproaches.github.io/marea/reference/ea.subset.spatial.md)
for a filtering helper function.
