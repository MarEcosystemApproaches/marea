# Understanding Generic EA Data Classes

## Understanding Data Classes in `marea`

### Introduction

The `marea` package is built around robust S4 class objects:  
- **ea_data** for time series/tabular data  
- **ea_spatial** for spatio-temporal or gridded data

This makes all `marea` datasets work the same – easy for plotting,
inspecting, and analysis.

------------------------------------------------------------------------

### What is a “Class” in `marea`?

An R “class” defines *how* an object behaves.  
Classes allow `marea` to automatically: - Store rich metadata with each
dataset - Print, summarize, or plot datasets using custom methods -
Guarantee consistent, predictable structures (for you and collaborators)

The two core classes are: - `ea_data`: 1D/tabular/time series
(e.g. abundances, environmental indices) - `ea_spatial`: gridded or
spatial/time-varying fields (e.g. maps of temperature)

------------------------------------------------------------------------

### ea_data: Clean, Consistent Time Series

All tabular time-series data in `marea` use the `ea_data` class.

#### Example: Load, Print, and Summarize

``` r
# Load grey seal abundance data
data("grey_seals")

# Print object summary (metadata, structure, preview)
ea.print(grey_seals)
```

    ## --- Ecosystem Approach (EA) Data Object --- 
    ## Class:  ea_data 
    ## Data Type:  Grey Seal Abundance 
    ## Species:   grey seal 
    ## Location:    ( Scotian Shelf  Region ) 
    ## Time Range:   1960  -  2021 
    ## Units:  number of seals 
    ## --------------------------------------------
    ## Data Preview:
    ##   year    lower median_value    upper
    ## 1 1960 1.652570     1.860824 2.396134
    ## 2 1961 2.032521     2.263192 2.809171
    ## 3 1962 2.407304     2.659018 3.214911
    ## 4 1963 2.700793     2.972982 3.541202
    ## 5 1964 2.785795     3.088971 3.684344
    ## 6 1965 3.126289     3.449040 4.053713

**Example output:**

— Ecosystem Approach (EA) Data Object — Class: ea_data Data Type:
Estimated abundance (number of seals, 1000s)

Data preview:

A tibble: 6 × 4 year low median high 1 1960 … … … 2 … … … …

#### Data summary

``` r
ea.summary(grey_seals)
```

    ## --- Summary of ea_data ---
    ## Metadata:
    ##   Data Type:  Grey Seal Abundance 
    ##   Species:  grey seal 
    ##   Location:   Sable Island 
    ##   Region:   Scotian Shelf 
    ##   Source:   den Heyer, C. E., Mosnier, A., Stenson, G. B., Lidgard, D. C., Bowen, W. D., & Hammill, M. O. (2024). Grey seal pup production in Canada (DFO Can. Sci. Advis. Sec. Res. Doc. 2023/078). Fisheries and Oceans Canada, Canadian Science Advisory Secretariat. 
    ## 
    ## Data Overview:
    ##   Time range:  1960  to  2021 
    ##   Number of observations:  62 
    ## 
    ## Summary of 'value' column (Units: number of seals):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.861   9.823  63.241 110.081 212.660 310.683

------------------------------------------------------------------------

### Plotting: One-line Visualization

Plotting works out-of-the-box and returns a `ggplot2` object:

``` r
# plot(grey_seals)
```

If columns like `"lower"` and `"upper"` exist, add ribbons easily:

``` r
# plot(grey_seals, style = "ribbon")
```

The style argument can be specified to return different plot types, this
can provide a range of reasonable defaults for different data sets.

Other styles: `"default"`, `"plain"`, `"anomaly"`, etc.

These are ggplot objects and are easy to add on to with the `+` syntax
(See advanced examples)

------------------------------------------------------------------------

### ea_spatial: Spatio-Temporal/Gridded Data

Spatial (or spatio-temporal) objects are stored as **ea_spatial**. These
have some flexibility and the data slot can house an sf objects, a stars
or terra object.

#### Example: Inspect a spatial object

``` r
data("glorys_bottom_temperature")
ea.summary(glorys_bottom_temperature)
```

    ## --- Summary of ea_spatial Object ---
    ## Metadata:
    ##   Data Type:  Bottom Temperature 
    ##   Region:     Northwest Atlantic 
    ##   Time:       monthly 
    ##   Source:     CMEMS Global Ocean Physics Reanalysis 
    ## 
    ## Spatial Information (from sf):
    ## Geometry set for 699770 features 
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -67.70833 ymin: 40.04166 xmax: -54.875 ymax: 47.79167
    ## Geodetic CRS:  WGS 84 (CRS84)
    ## First 5 geometries:

    ## POLYGON ((-67.70833 40.04166, -67.625 40.04166,...
    ## POLYGON ((-67.70833 40.04166, -67.625 40.04166,...
    ## POLYGON ((-67.70833 40.04166, -67.625 40.04166,...
    ## POLYGON ((-67.70833 40.04166, -67.625 40.04166,...
    ## POLYGON ((-67.70833 40.04166, -67.625 40.04166,...

    ## 
    ## Time periods:  2020-8, 2020-9, 2020-10, 2020-11, 2020-12, 2021-1, 2021-2, 2021-3, 2021-4, 2021-5, 2021-6, 2021-7, 2021-8, 2021-9, 2021-10, 2021-11, 2021-12, 2022-1, 2022-2, 2022-3, 2022-4, 2022-5, 2022-6, 2022-7, 2022-8, 2022-9, 2022-10, 2022-11, 2022-12, 2023-1, 2023-2, 2023-3, 2023-4, 2023-5, 2023-6, 2023-7, 2023-8, 2023-9, 2023-10, 2023-11, 2023-12, 2024-1, 2024-2, 2024-3, 2024-4, 2024-5, 2024-6, 2024-7, 2024-8, 2024-9, 2024-10, 2024-11, 2024-12, 2025-1, 2025-2, 2025-3, 2025-4, 2025-5 
    ## Number of time periods:  58 
    ## 
    ## Summary of 'value' column (Units: degC):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -2.690   1.845   3.018   4.266   6.298  23.280

*Output includes metadata, spatial bounds, etc.*

#### Plot

``` r
# plot(glorys_bottom_temperature)
```

The style argument can be used to specify some reasonable spatial
default styles. There are also many other arguments for specifying
details about coastline objects, regions, and other considerations. See
`?plot.ea_spatial` for more details. Other options: -
`style = "contour"` (contour lines) - `style = "bubble"` (bubbles by
value)

------------------------------------------------------------------------

### Interoperability: Converting from `pacea` and Other Packages

`marea` works great with external datasets such as those from `pacea`
(Pacific ecosystem data).

If you have a `pacea` time-series (pacea-index or pacea-biomass):

``` r
library(pacea)
data(npgo) # Example: North Pacific Gyre Oscillation
marea_npgo <- as_ea_data(
  x = npgo,
  value_col = c("anomaly")
)
class(marea_npgo)
```

    ## [1] "ea_data"
    ## attr(,"package")
    ## [1] "marea"

``` r
ea.print(marea_npgo)
```

    ## --- Ecosystem Approach (EA) Data Object --- 
    ## Class:  ea_data 
    ## Data Type:  North Pacific Gyre Oscillation 
    ## Location:    ( Not specified  Region ) 
    ## Time Range:   1950  -  2025 
    ## Units:   
    ## --------------------------------------------
    ## Data Preview:
    ##   year month anomaly_value
    ## 1 1950     1    -2.1883951
    ## 2 1950     2    -1.4458314
    ## 3 1950     3    -0.9650357
    ## 4 1950     4    -0.8587880
    ## 5 1950     5    -0.6340822
    ## 6 1950     6    -0.5809843

For spatial objects, use:

``` r
library(pacea)
data("hake_total_biomass_age_1_2025")

hake_biomass_ea <- as_ea_data(
  hake_total_biomass_age_1_2025,
  value_col = c("median"),
  source = "pacea",
  time_descriptor = "2025",
  details = "Converted using marea::as_ea_data() July 2025"
)
print(hake_biomass_ea)
```

    ## An object of class "ea_data"
    ## Slot "meta":
    ## $data_type
    ## [1] "Pacific Hake total biomass of age-1 fish (thousand t)"
    ## 
    ## $region
    ## [1] "Not specified"
    ## 
    ## $location_descriptor
    ## [1] "Not specified"
    ## 
    ## $units
    ## [1] ""
    ## 
    ## $species
    ## [1] NA
    ## 
    ## $source_citation
    ## [1] "Not specified"
    ## 
    ## $original_value_col
    ## [1] "median"
    ## 
    ## $time_descriptor
    ## [1] "2025"
    ## 
    ## $source
    ## [1] "pacea"
    ## 
    ## $details
    ## [1] "Converted using marea::as_ea_data() July 2025"
    ## 
    ## 
    ## Slot "data":
    ## # A tibble: 60 × 2
    ##     year median_value
    ##    <dbl>        <dbl>
    ##  1  1966        140. 
    ##  2  1967        122. 
    ##  3  1968        362. 
    ##  4  1969        232. 
    ##  5  1970         53.8
    ##  6  1971        693. 
    ##  7  1972         62.5
    ##  8  1973         40.6
    ##  9  1974        454. 
    ## 10  1975         16.6
    ## # ℹ 50 more rows

------------------------------------------------------------------------

### Creating your own ea objects

While marea provides many built-in datasets, you’ll often want to create
your own objects from scratch. This section demonstrates how to build
ea_data and ea_spatial objects using the constructor functions.

#### Create an ea_data Object

The
[`ea_data()`](https://marecosystemapproaches.github.io/marea/reference/ea_data.md)
constructor allows you to create time series objects from any data
frame. Here’s how:

``` r
# Create a simple temperature dataset
temp_data <- data.frame(
  year = 2010:2020,
  avg_temp = c(8.2, 8.5, 8.1, 8.9, 9.2, 8.8, 9.1, 8.7, 9.0, 8.6, 9.3),
  station = "Halifax-2"
)

# Create ea_data object
halifax_temp <- ea_data(
  data = temp_data,
  value_col = "avg_temp", # Specify which column contains the values
  data_type = "Bottom Temperature",
  region = "Maritimes",
  location_descriptor = "Halifax Line Station 2",
  units = "°C",
  source_citation = "DFO Maritimes Region Monitoring Program"
)

ea.print(halifax_temp)
```

    ## --- Ecosystem Approach (EA) Data Object --- 
    ## Class:  ea_data 
    ## Data Type:  Bottom Temperature 
    ## Location:    ( Maritimes  Region ) 
    ## Time Range:   2010  -  2020 
    ## Units:  °C 
    ## --------------------------------------------
    ## Data Preview:
    ##   year avg_temp_value   station
    ## 1 2010            8.2 Halifax-2
    ## 2 2011            8.5 Halifax-2
    ## 3 2012            8.1 Halifax-2
    ## 4 2013            8.9 Halifax-2
    ## 5 2014            9.2 Halifax-2
    ## 6 2015            8.8 Halifax-2

#### Advanced ea_data with additional metadata

``` r
# Fisheries catch data with uncertainty
catch_data <- data.frame(
  year = 2015:2022,
  landings = c(1250, 1180, 1320, 1450, 1280, 1390, 1220, 1310),
  vessel_count = c(45, 42, 48, 51, 46, 49, 44, 47)
)

# Create object with additional metadata
lobster_catch <- ea_data(
  data = catch_data,
  value_col = "landings",
  data_type = "Commercial Landings",
  region = "Maritimes",
  location_descriptor = "LFA 34",
  units = "tonnes",
  species = "American Lobster",
  source_citation = "DFO INTERNAL",
  # Additional custom metadata
  fishing_season = "November-May",
  assessment_year = 2023,
  stock_status = "Healthy"
)

ea.print(lobster_catch)
```

    ## --- Ecosystem Approach (EA) Data Object --- 
    ## Class:  ea_data 
    ## Data Type:  Commercial Landings 
    ## Species:   American Lobster 
    ## Location:    ( Maritimes  Region ) 
    ## Time Range:   2015  -  2022 
    ## Units:  tonnes 
    ## --------------------------------------------
    ## Data Preview:
    ##   year landings_value vessel_count
    ## 1 2015           1250           45
    ## 2 2016           1180           42
    ## 3 2017           1320           48
    ## 4 2018           1450           51
    ## 5 2019           1280           46
    ## 6 2020           1390           49

``` r
ea.summary(lobster_catch)
```

    ## --- Summary of ea_data ---
    ## Metadata:
    ##   Data Type:  Commercial Landings 
    ##   Species:  American Lobster 
    ##   Location:   LFA 34 
    ##   Region:   Maritimes 
    ##   Source:   DFO INTERNAL 
    ## 
    ## Data Overview:
    ##   Time range:  2015  to  2022 
    ##   Number of observations:  8 
    ## 
    ## Summary of 'value' column (Units: tonnes):
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1180    1242    1295    1300    1338    1450

#### Create an ea_spatial object

The
[`ea_spatial()`](https://marecosystemapproaches.github.io/marea/reference/ea_spatial.md)
constructor creates spatial objects from sf data. Here’s how to build
one:

``` r
library(sf)
```

    ## Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

``` r
# Create a simple spatial grid
grid_points <- expand.grid(
  lon = seq(-66, -64, by = 0.5),
  lat = seq(43, 45, by = 0.5)
)

# Convert to sf object
spatial_data <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326)

# Add some simulated chlorophyll data
spatial_data$chl_concentration <- runif(nrow(spatial_data), 0.5, 3.2)
spatial_data$depth_zone <- "surface"

# Create ea_st object
chl_spatial <- ea_spatial(
  data = spatial_data,
  value_col = "chl_concentration",
  data_type = "Chlorophyll-a Concentration",
  region = "Maritimes",
  time_descriptor = "July 2023 Survey",
  units = "mg/m³",
  source_citation = "DFO Ecosystem Survey Program"
)

ea.print(chl_spatial)
```

    ## --- Ecosystem Approach Spatio-Temporal (ea_spatial) Object ---
    ## Data Type:       Chlorophyll-a Concentration 
    ## Time:            July 2023 Survey 
    ## Region:          Maritimes 
    ## Units:          mg/m³ (in 'value' column, originally 'chl_concentration')
    ## -----------------------------------------------------------
    ##              geometry     value depth_zone
    ## 1      POINT (-66 43) 0.7180254    surface
    ## 2    POINT (-65.5 43) 2.7526992    surface
    ## 3      POINT (-65 43) 2.1220544    surface
    ## 4    POINT (-64.5 43) 0.9244628    surface
    ## 5      POINT (-64 43) 0.5199785    surface
    ## 6    POINT (-66 43.5) 1.7592624    surface
    ## 7  POINT (-65.5 43.5) 1.8439989    surface
    ## 8    POINT (-65 43.5) 1.2823716    surface
    ## 9  POINT (-64.5 43.5) 2.4787814    surface
    ## 10   POINT (-64 43.5) 2.5858081    surface
    ## 11     POINT (-66 44) 2.8614218    surface
    ## 12   POINT (-65.5 44) 0.9723397    surface
    ## 13     POINT (-65 44) 0.5924516    surface
    ## 14   POINT (-64.5 44) 1.3650415    surface
    ## 15     POINT (-64 44) 1.5862862    surface
    ## 16   POINT (-66 44.5) 1.0283086    surface
    ## 17 POINT (-65.5 44.5) 1.5895529    surface
    ## 18   POINT (-65 44.5) 0.6718859    surface
    ## 19 POINT (-64.5 44.5) 1.5494935    surface
    ## 20   POINT (-64 44.5) 3.1339792    surface
    ## 21     POINT (-66 45) 1.2827092    surface
    ## 22   POINT (-65.5 45) 2.3316272    surface
    ## 23     POINT (-65 45) 2.4853629    surface
    ## 24   POINT (-64.5 45) 1.0290832    surface
    ## 25     POINT (-64 45) 3.1474571    surface

### Accessing Data and Metadata: The S4 Structure

Once you have an ea_data object, you might want to extract specific
pieces of information, like the raw data frame or a single metadata
value. Because ea_data is an S4 object, there are two primary ways to do
this: the standard @ operator for direct slot access, and a custom \[\[
method for more flexible and convenient access.

An ea_data object has two main “slots”:

- meta: A list that holds all the metadata (e.g., region, units,
  source_citation).

- data: A tibble that contains the time series data (with year, value,
  and other columns).

#### Using the @ Operator (Direct Slot Access)

The @ operator is the standard way to access slots in any S4 object. You
can use it to pull out the entire meta list or the data tibble.

``` r
# Use the halifax_temp object created earlier
# Access the entire metadata list
halifax_temp@meta
```

    ## $data_type
    ## [1] "Bottom Temperature"
    ## 
    ## $region
    ## [1] "Maritimes"
    ## 
    ## $location_descriptor
    ## [1] "Halifax Line Station 2"
    ## 
    ## $units
    ## [1] "°C"
    ## 
    ## $species
    ## [1] NA
    ## 
    ## $source_citation
    ## [1] "DFO Maritimes Region Monitoring Program"
    ## 
    ## $original_value_col
    ## [1] "avg_temp"

``` r
# Access the entire data tibble
halifax_temp@data
```

    ## # A tibble: 11 × 3
    ##     year avg_temp_value station  
    ##    <int>          <dbl> <chr>    
    ##  1  2010            8.2 Halifax-2
    ##  2  2011            8.5 Halifax-2
    ##  3  2012            8.1 Halifax-2
    ##  4  2013            8.9 Halifax-2
    ##  5  2014            9.2 Halifax-2
    ##  6  2015            8.8 Halifax-2
    ##  7  2016            9.1 Halifax-2
    ##  8  2017            8.7 Halifax-2
    ##  9  2018            9   Halifax-2
    ## 10  2019            8.6 Halifax-2
    ## 11  2020            9.3 Halifax-2

This method is direct and explicit but requires you to know the exact
slot names. To get a single metadata item, you would need to chain
operators, like `halifax_temp@meta$region`.

#### Using the \[\[ Method (Convenient Access)

To make life easier, marea provides a custom \[\[ method. This powerful
shortcut lets you access the entire meta or data slots, as well as
individual items within them, using a single, consistent syntax.

The \[\[ method first checks if you are asking for “meta” or “data”. If
not, it checks if the name you provided exists in the meta slot. If it’s
not there, it finally checks if it’s a column name in\` the data slot.

``` r
# Get the entire meta list (same as @meta)
halifax_temp[["meta"]]
```

    ## $data_type
    ## [1] "Bottom Temperature"
    ## 
    ## $region
    ## [1] "Maritimes"
    ## 
    ## $location_descriptor
    ## [1] "Halifax Line Station 2"
    ## 
    ## $units
    ## [1] "°C"
    ## 
    ## $species
    ## [1] NA
    ## 
    ## $source_citation
    ## [1] "DFO Maritimes Region Monitoring Program"
    ## 
    ## $original_value_col
    ## [1] "avg_temp"

``` r
# Get a specific metadata item
halifax_temp[["region"]]
```

    ## [1] "Maritimes"

``` r
# Get the entire data tibble (same as @data)
halifax_temp[["data"]]
```

    ## # A tibble: 11 × 3
    ##     year avg_temp_value station  
    ##    <int>          <dbl> <chr>    
    ##  1  2010            8.2 Halifax-2
    ##  2  2011            8.5 Halifax-2
    ##  3  2012            8.1 Halifax-2
    ##  4  2013            8.9 Halifax-2
    ##  5  2014            9.2 Halifax-2
    ##  6  2015            8.8 Halifax-2
    ##  7  2016            9.1 Halifax-2
    ##  8  2017            8.7 Halifax-2
    ##  9  2018            9   Halifax-2
    ## 10  2019            8.6 Halifax-2
    ## 11  2020            9.3 Halifax-2

``` r
# Get a specific data column (vector)
halifax_temp[["year"]]
```

    ##  [1] 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020

This approach is generally recommended as it provides a single,
intuitive interface for retrieving almost any piece of information from
an ea_data object without needing to remember the underlying @ syntax.

### Key Constructor Parameters

Both constructors share common metadata fields:

#### Required parameters:

- data: Your data frame (for ea_data) or sf/stars/terra spatraster
  object (for ea_spatial)

- value_col: Name of the column containing the primary values

- data_type: Description of what the data represents

- region: Geographic region or NAFO/DFO area name

- units: Units for the value column

- source_citation: Where the data came from

#### ea_data specific:

- location_descriptor: Specific location identifier

- species: Species name (optional, for biological data)

#### ea_spatial specific:

time_descriptor: Description of the temporal aspect

#### Additional metadata:

- You can add any custom metadata fields as named arguments (e.g.,
  fishing_season, assessment_year)

### Tips for creating objects

1.  Value column naming: The constructor automatically renames your
    specified column to value internally, so all methods work
    consistently.

2.  Metadata travels: All metadata is preserved through subsetting and
    plotting operations.

3.  Validation: Constructors validate inputs and provide helpful error
    messages.

4.  Extensibility: Add custom metadata fields for your specific use
    case.

### Why Use These Classes?

Using robust S4 classes brings multiple benefits: - All datasets behave
the same way (easy to plot, inspect, or summarize) - Metadata (units,
region, source, etc.) always travel with the data - You can use base R
summary/print/plot methods or build your own analyses on top

------------------------------------------------------------------------

### Learn More

- The main
  [README](https://marecosystemapproaches.github.io/marea/README.md) has
  a quick start

- ### Plotting EA classes vignette

### Citing `marea`

Please cite the package in your work. For citation text:

``` r
citation("marea")
```

    ## To cite marea in publications use:
    ## 
    ##   Jamie C. Tam, Benoit Casault, Stephanie Clay et al. (). marea:
    ##   Maritime Ecosystem Approach R Package. R package version 1.0.0.9000.
    ##   https://doi.org/10.5281/zenodo.15706086
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {marea: Maritime Ecosystem Approach R Package},
    ##     author = {Jamie C. Tam and Benoit Casault and Stephanie Clay and Adam Cook and Remi Daigle and Andrew M. Edwards and Jaimie Harbin and Brad Hubley and Keith David and Mike McMahon and Emily A. O'Grady},
    ##     note = {R package version 1.0.0.9000},
    ##     url = {https://github.com/MarEcosystemApproaches/marea},
    ##     doi = {10.5281/zenodo.15706086},
    ##   }
    ## 
    ## We appreciate citations as they help us secure funding for continued
    ## development.

------------------------------------------------------------------------

*This vignette was generated using the `marea` source code
(2025-07-24).*

------------------------------------------------------------------------
