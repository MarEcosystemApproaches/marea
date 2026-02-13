# Filter an `ea_data` Object by Column and Value

A convenient helper function to filter the `data` slot of an `ea_data`
object based on matching values in a specified column.

## Usage

``` r
ea.subset(x, column, value)
```

## Arguments

- x:

  An `ea_data` object.

- column:

  A character string naming the column in the `data` slot to filter by.

- value:

  A value or vector of values to match in the specified column.

## Value

A new `ea_data` object containing only the matching rows.

## Examples

``` r
df <- data.frame(
  year = rep(2000:2002, 2),
  temp_c = rnorm(6),
  group = rep(c("A", "B"), each = 3)
)
obj <- ea_data(df,
  value_col = c("temp_c"), data_type = "temperature",
  region = "Test Region", location_descriptor = "surface",
  units = "°C"
)

# Filter for a single year
ea.subset(obj, "year", 2001)
#> An object of class "ea_data"
#> Slot "meta":
#> $data_type
#> [1] "temperature"
#> 
#> $region
#> [1] "Test Region"
#> 
#> $location_descriptor
#> [1] "surface"
#> 
#> $units
#> [1] "°C"
#> 
#> $species
#> [1] NA
#> 
#> $source_citation
#> [1] "No citation provided"
#> 
#> $original_value_col
#> [1] "temp_c"
#> 
#> 
#> Slot "data":
#> # A tibble: 2 × 3
#>    year temp_c_value group
#>   <int>        <dbl> <chr>
#> 1  2001      -0.0526 A    
#> 2  2001       0.468  B    
#> 

# Filter for specific groups
ea.subset(obj, "group", "B")
#> An object of class "ea_data"
#> Slot "meta":
#> $data_type
#> [1] "temperature"
#> 
#> $region
#> [1] "Test Region"
#> 
#> $location_descriptor
#> [1] "surface"
#> 
#> $units
#> [1] "°C"
#> 
#> $species
#> [1] NA
#> 
#> $source_citation
#> [1] "No citation provided"
#> 
#> $original_value_col
#> [1] "temp_c"
#> 
#> 
#> Slot "data":
#> # A tibble: 3 × 3
#>    year temp_c_value group
#>   <int>        <dbl> <chr>
#> 1  2000       -0.914 B    
#> 2  2001        0.468 B    
#> 3  2002        0.363 B    
#> 
```
