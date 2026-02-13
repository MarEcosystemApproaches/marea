# An S4 Class for Environmental Assessment Data

The `ea_data` class is a container for environmental assessment time
series data. It encapsulates a data frame with the core data and a list
of metadata, ensuring that essential columns (`year`, `value`) are
present and providing a standardized interface for accessing and
manipulating the data.

## Slots

- `meta`:

  A named list containing metadata. Expected fields include `data_type`,
  `region`, `location_descriptor`, `units`, `species`,
  `source_citation`, and `original_value_col`.

- `data`:

  A `data.frame` or `tibble` containing the time series data. Must
  include at least a `year` column and a `value` column.

## See also

[`ea_data`](https://marecosystemapproaches.github.io/marea/reference/ea_data.md)
for the constructor function.

[`ea.subset`](https://marecosystemapproaches.github.io/marea/reference/ea.subset.md)
for a filtering helper function.
