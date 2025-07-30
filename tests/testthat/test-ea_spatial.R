# Setup: Load necessary libraries
library(testthat)
library(methods) 

# --- Helper functions for test setup ---

# Helper function to create a basic sf object
create_test_sf <- function(n = 3, value_col_name = "temp_val",
                           data_type = "Temperature",
                           region = "Test Region",
                           time_descriptor = "Annual Avg",
                           units = "degC",
                           extra_cols = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    testthat::skip("sf package not installed")
  }
  pts <- lapply(1:n, function(i) sf::st_point(c(-63 + i, 44 + i)))
  df <- data.frame(id = 1:n)
  df[[value_col_name]] <- rnorm(n, mean = 15, sd = 2)
  if (!is.null(extra_cols)) {
    for (col_name in names(extra_cols)) {
      df[[col_name]] <- extra_cols[[col_name]]
    }
  }
  sf_obj <- sf::st_sf(df, geometry = sf::st_sfc(pts, crs = 4326))
  
  ea_spatial(
    data = sf_obj,
    value_col = value_col_name,
    data_type = data_type,
    region = region,
    time_descriptor = time_descriptor,
    units = units
  )
}

# Helper function to create a basic stars object
create_test_stars <- function(nx = 2, ny = 2, value_col_name = "raster_val",
                              data_type = "Raster Temp",
                              region = "Test Grid",
                              time_descriptor = "July 2021",
                              units = "C",
                              extra_layers = NULL) {
  if (!requireNamespace("stars", quietly = TRUE)) {
    testthat::skip("stars package not installed")
  }
  
  # Create the base stars object
  mat <- matrix(rnorm(nx * ny, mean = 20, sd = 5), nrow = nx, ncol = ny)
  st_obj <- stars::st_as_stars(mat)
  names(st_obj) <- value_col_name
  st_obj <- stars::st_set_dimensions(st_obj, names = c("x", "y"))
  
  # Add extra layers as new, separate attributes
  if (!is.null(extra_layers)) {
    for (layer_name in names(extra_layers)) {
      # Create the new layer as a stars object
      new_layer_mat <- matrix(extra_layers[[layer_name]], nrow = nx, ncol = ny)
      new_layer_st <- stars::st_as_stars(new_layer_mat)
      names(new_layer_st) <- layer_name 
      st_obj <- c(st_obj, new_layer_st)
    }
  }
  
  # st_obj has multiple attributes: "raster_val", "category", etc.
  ea_spatial(
    data = st_obj,
    value_col = value_col_name,
    data_type = data_type,
    region = region,
    time_descriptor = time_descriptor,
    units = units
  )
}

# Helper function to create a basic SpatRaster object
create_test_spatraster <- function(ncols = 2, nrows = 2, value_col_name = "spat_val",
                                   data_type = "SpatRaster Data",
                                   region = "SpatRaster Area",
                                   time_descriptor = "Monthly Avg",
                                   units = "units",
                                   extra_layers = NULL) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    testthat::skip("terra package not installed")
  }
  r <- terra::rast(ncols = ncols, nrows = nrows,
                   xmin = 0, xmax = ncols, ymin = 0, ymax = nrows)
  terra::values(r) <- rnorm(ncols * nrows, mean = 50, sd = 10)
  names(r) <- value_col_name
  
  if (!is.null(extra_layers)) {
    for (layer_name in names(extra_layers)) {
      r_new <- r
      terra::values(r_new) <- extra_layers[[layer_name]]
      names(r_new) <- layer_name
      r <- c(r, r_new)
    }
  }
  
  ea_spatial(
    data = r,
    value_col = value_col_name,
    data_type = data_type,
    region = region,
    time_descriptor = time_descriptor,
    units = units
  )
}

# --- Tests for ea_spatial S4 Class Definition ---

test_that("ea_spatial S4 class definition is correct", {
  # Requires at least one supported spatial package to create a valid object
  # for validity check.
  if (requireNamespace("sf", quietly = TRUE)) {
    sf_obj <- sf::st_sf(data.frame(value = 1), geometry = sf::st_sfc(sf::st_point(c(0,0)), crs=4326))
    obj <- methods::new("ea_spatial", meta = list(a = 1), data = sf_obj)
    expect_s4_class(obj, "ea_spatial")
    expect_true(is.list(obj@meta))
    expect_true(inherits(obj@data, "sf"))
  } else {
    testthat::skip("sf package not installed, cannot fully test class definition.")
  }
})

# --- Tests for ea_spatial Constructor ---

test_that("ea_spatial constructor creates valid object with sf data", {
  obj <- create_test_sf()
  expect_s4_class(obj, "ea_spatial")
  expect_true(all(c("meta", "data") %in% slotNames(obj)))
  
  expect_true(is.list(obj@meta))
  expect_true(all(c("data_type", "region", "time_descriptor", "units",
                    "source_citation", "original_value_col") %in% names(obj@meta)))
  expect_equal(obj@meta$data_type, "Temperature")
  expect_equal(obj@meta$original_value_col, "temp_val")
  
  expect_s3_class(obj@data, "sf")
  expect_true("value" %in% names(obj@data))
  expect_true(inherits(obj@data$geometry, "sfc"))
})

test_that("ea_spatial constructor creates valid object with stars data", {
  obj <- create_test_stars()
  expect_s4_class(obj, "ea_spatial")
  expect_true(inherits(obj@data, "stars"))
  expect_true("value" %in% names(obj@data))
  expect_equal(obj@meta$original_value_col, "raster_val")
})

test_that("ea_spatial constructor creates valid object with SpatRaster data", {
  obj <- create_test_spatraster()
  expect_s4_class(obj, "ea_spatial")
  expect_true(inherits(obj@data, "SpatRaster"))
  expect_true("value" %in% names(obj@data)) # Names for SpatRaster layers
  expect_equal(obj@meta$original_value_col, "spat_val")
})


test_that("ea_spatial constructor handles optional arguments and extra metadata", {
  if (!requireNamespace("sf", quietly = TRUE)) {
    testthat::skip("sf package not installed")
  }
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
  obj <- ea_spatial(
    data = spatial_data,
    value_col = "chl_concentration",
    data_type = "Chlorophyll-a Concentration",
    region = "Maritimes",
    time_descriptor = "July 2023 Survey",
    units = "mg/m³",
    source_citation = "DFO Ecosystem Survey Program",
    project_name = 'Global Marine Census'
  )
  expect_equal(obj@meta$source_citation, "DFO Ecosystem Survey Program")
  expect_equal(obj@meta$project_name, "Global Marine Census")
})

test_that("ea_spatial constructor throws errors for invalid data type", {
  expect_error(
    ea_spatial(
      data = data.frame(x = 1),
      value_col = "x",
      data_type = "t", region = "r", time_descriptor = "ti", units = "u"
    ),
    "`data` must be of class `sf`, `stars`, or `SpatRaster`.",
    fixed = TRUE
  )
})

test_that("ea_spatial constructor throws errors for missing value_col", {
  if (requireNamespace("sf", quietly = TRUE)) {
    sf_df_no_val <- sf::st_sf(data.frame(x = 1), geometry = sf::st_sfc(sf::st_point(c(0,0)), crs=4326))
    expect_error(
      ea_spatial(
        data = sf_df_no_val,
        value_col = "non_existent_col",
        data_type = "t", region = "r", time_descriptor = "ti", units = "u"
      ),
      "Column/layer 'non_existent_col' not found in the data object.",
      fixed = TRUE
    )
  }
})

test_that("ea_spatial constructor throws error for non-numeric value_col in sf", {
  if (requireNamespace("sf", quietly = TRUE)) {
    sf_df_char_val <- sf::st_sf(data.frame(char_val = "A"), geometry = sf::st_sfc(sf::st_point(c(0,0)), crs=4326))
    expect_error(
      ea_spatial(
        data = sf_df_char_val,
        value_col = "char_val",
        data_type = "t", region = "r", time_descriptor = "ti", units = "u"
      ),
      "Column 'char_val' must be numeric for sf objects.",
      fixed = TRUE
    )
  }
})

test_that("ea_spatial constructor correctly renames value column/layer", {
  obj_sf <- create_test_sf(value_col_name = "my_custom_value")
  expect_true("value" %in% names(obj_sf@data))
  expect_false("my_custom_value" %in% names(obj_sf@data))
  expect_equal(obj_sf@meta$original_value_col, "my_custom_value")
  
  obj_stars <- create_test_stars(value_col_name = "my_stars_value")
  expect_true("value" %in% names(obj_stars@data))
  expect_false("my_stars_value" %in% names(obj_stars@data))
  expect_equal(obj_stars@meta$original_value_col, "my_stars_value")
  
  obj_spatraster <- create_test_spatraster(value_col_name = "my_raster_value")
  expect_true("value" %in% names(obj_spatraster@data))
  expect_false("my_raster_value" %in% names(obj_spatraster@data))
  expect_equal(obj_spatraster@meta$original_value_col, "my_raster_value")
})

# --- `[[` Accessor Tests ---

test_that("[[ operator extracts 'meta' slot", {
  obj <- create_test_sf()
  meta_list <- obj[["meta"]]
  expect_type(meta_list, "list")
  expect_equal(meta_list$data_type, "Temperature")
})

test_that("[[ operator extracts 'data' slot", {
  obj <- create_test_sf()
  data_obj <- obj[["data"]]
  expect_s3_class(data_obj, "sf")
})

test_that("[[ operator extracts metadata fields directly", {
  obj <- create_test_sf(data_type = "Depth", region = "Atlantic")
  expect_equal(obj[["data_type"]], "Depth")
  expect_equal(obj[["region"]], "Atlantic")
})

test_that("[[ operator extracts data columns directly (sf)", {
  obj_sf <- create_test_sf(n = 2, extra_cols = list(sample_id = c("A", "B")))
  expect_equal(obj_sf[["sample_id"]], c("A", "B"))
  expect_type(obj_sf[["value"]], "double")
  
})

test_that("[[ operator extracts data columns directly (stars)", {
  obj_stars <- create_test_stars()
  expect_type(obj_stars[["value"]], "double")
})

test_that("[[ operator extracts data layers directly (SpatRaster)", {
  obj_spatraster <- create_test_spatraster()
  expect_type(obj_spatraster[["value"]], "double")
})

test_that("[[ operator throws error for non-existent elements", {
  obj <- create_test_sf()
  expect_error(
    obj[["non_existent_field"]],
    "Element 'non_existent_field' not found in 'meta' or 'data'",
    fixed = TRUE
  )
})

# --- `[` Subsetting Tests ---

test_that("[ operator subsets sf rows correctly (numeric index)", {
  obj_sf <- create_test_sf(n = 5)
  subset_obj <- obj_sf[1:3, ]
  expect_s4_class(subset_obj, "ea_spatial")
  expect_s3_class(subset_obj@data, "sf")
  expect_equal(nrow(subset_obj@data), 3)
  expect_equal(subset_obj@meta, obj_sf@meta)
})

test_that("[ operator subsets sf rows correctly (logical index)", {
  obj_sf <- create_test_sf(n = 5, extra_cols = list(active = c(T,F,T,F,T)))
  subset_obj <- obj_sf[obj_sf@data$active, ]
  expect_equal(nrow(subset_obj@data), 3)
})

test_that("[ operator subsets stars objects correctly ", {
  obj_stars <- create_test_stars()
  subset_obj <- obj_stars[1:1, ] # Subset x-dimension
  expect_equal(stars::st_dimensions(subset_obj@data)$X1$to, 2)
  expect_equal(names(subset_obj@data), names(obj_stars@data)) # All layers retained
})

test_that("[ operator subsets SpatRaster objects correctly (using terrra method)", {
  obj_spatraster <- create_test_spatraster(ncols = 3, nrows = 3, extra_layers = list(type = rep(c(1,2,3), each=3)))
  subset_obj_layers <- obj_spatraster[ , 2:3] # Subset layers (j for layers, not columns)
  expect_s4_class(subset_obj_layers, "ea_spatial")
  expect_true(inherits(subset_obj_layers@data, "SpatRaster"))
  expect_equal(terra::nlyr(subset_obj_layers@data), 2)
  expect_equal(names(subset_obj_layers@data), c("value", "type")) # value is first, type is second in original
  
  # For spatial subsetting (cropping)
  if (requireNamespace("terra", quietly = TRUE)) {
    ext <- terra::ext(0, 1, 0, 3) # Subset a region
    subset_obj_spatial <- obj_spatraster[as.vector(ext)]
    expect_s4_class(subset_obj_spatial, "ea_spatial")
    expect_true(inherits(subset_obj_spatial@data, "SpatRaster"))
    expect_equal(terra::ncol(subset_obj_spatial@data), 1)
    expect_equal(terra::xmin(subset_obj_spatial@data), 0)
  }
})

test_that("[ operator warns and ignores column subsetting (j) for sf/stars", {
  obj_sf <- create_test_sf()
  expect_warning(
    subset_obj <- obj_sf[, "value"],
    "Column subsetting "
  )
  expect_equal(names(subset_obj@data), names(obj_sf@data)) # All columns retained
  expect_equal(nrow(subset_obj@data), nrow(obj_sf@data))
})

test_that("[ operator preserves object structure when subsetting to zero rows/features", {
  obj_sf <- create_test_sf(n = 5)
  subset_obj_sf <- obj_sf[numeric(0), ]
  expect_equal(nrow(subset_obj_sf@data), 0)
  expect_equal(names(subset_obj_sf@data), names(obj_sf@data))
  expect_equal(subset_obj_sf@meta, obj_sf@meta)
  
  obj_stars <- create_test_stars(nx = 1, ny = 1)
  # stars doesn't subset to 0 rows easily like sf, usually `stars::st_empty()` for empty
  # or specific dimension subsetting that makes it empty.
  # For now, let's just confirm it doesn't error when `i` makes it empty if it supports it.
  # `obj_stars[0, ]` might not work as expected in stars for 0 rows.
  # Testing `obj_stars[numeric(0), ]` might throw an error or unexpected result if stars doesn't map it.
  # It's better to ensure it produces an empty stars object if the underlying `[` does.
  # Given `stars::st_as_stars(matrix(numeric(0), 0,0))` is empty.
  # Let's rely on the underlying `stars` implementation, or provide a custom subsetting.
  # For now, test if it doesn't error.
  expect_no_error({
    subset_obj_stars <- obj_stars[1:0, ] # Subset to empty slice on x dim
    expect_s4_class(subset_obj_stars, "ea_spatial")
  })
})


# --- `ea.subset.spatial` Helper Function Tests ---

test_that("ea.subset.spatial filters sf objects correctly by a single attribute", {
  obj_sf <- create_test_sf(n = 5, extra_cols = list(type = c("A", "B", "A", "C", "B")))
  filtered_obj <- ea.subset.spatial(obj_sf, "type", "A")
  expect_s4_class(filtered_obj, "ea_spatial")
  expect_s3_class(filtered_obj@data, "sf")
  expect_equal(nrow(filtered_obj@data), 2)
  expect_equal(sf::st_drop_geometry(filtered_obj@data)$type, c("A", "A"))
  expect_equal(filtered_obj@meta, obj_sf@meta)
})

test_that("ea.subset.spatial filters sf objects correctly by multiple attributes", {
  obj_sf <- create_test_sf(n = 5, extra_cols = list(type = c("A", "B", "A", "C", "B")))
  filtered_obj <- ea.subset.spatial(obj_sf, "type", c("A", "C"))
  expect_equal(nrow(filtered_obj@data), 3)
  expect_equal(sf::st_drop_geometry(filtered_obj@data)$type, c("A", "A", "C"))
})

test_that("ea.subset.spatial filters stars objects correctly by attribute layer", {
  obj_stars <- create_test_stars( extra_layers = list(category = c("X", "Y", "X", "Z")))
  filtered_obj <- ea.subset.spatial(obj_stars, "category", "X")
  expect_s4_class(filtered_obj, "ea_spatial")
  expect_true(inherits(filtered_obj@data, "stars"))
  expect_equal(sum(filtered_obj@data$value %in% c(NA_real_)), 2) # 2 cells should be NA
  expect_equal(filtered_obj@meta, obj_stars@meta)
})


test_that("ea.subset.spatial filters SpatRaster objects correctly by attribute layer", {
  obj_spatraster <- create_test_spatraster(ncols = 2, nrows = 2, extra_layers = list(zone = c(1, 2, 1, 2)))
  filtered_obj <- ea.subset.spatial(obj_spatraster, "zone", 1)
  expect_s4_class(filtered_obj, "ea_spatial")
  expect_true(inherits(filtered_obj@data, "SpatRaster"))
  expect_equal(as.vector(terra::values(filtered_obj@data$zone)), c(1, NA, 1, NA)) # Matching cells retain value, others NA
  expect_equal(sum(is.na(terra::values(filtered_obj@data$value))), 2) # The `value` layer should also be masked
  expect_equal(filtered_obj@meta, obj_spatraster@meta)
})


test_that("ea.subset.spatial returns object with 0 features/cells if no matches", {
  obj_sf <- create_test_sf(n = 2, extra_cols = list(type = c("A", "B")))
  filtered_obj_sf <- ea.subset.spatial(obj_sf, "type", "Z")
  expect_equal(nrow(filtered_obj_sf@data), 0)
  
  obj_stars <- create_test_stars(nx = 1, ny = 1, extra_layers = list(type = c("A")))
  filtered_obj_stars <- ea.subset.spatial(obj_stars, "type", "Z")
  # stars will result in all NA if no match, not 0 dimensions
  expect_true(all(is.na(filtered_obj_stars@data$value)))
  expect_true(all(is.na(filtered_obj_stars@data$type)))
  
  obj_spatraster <- create_test_spatraster(ncols = 1, nrows = 1, extra_layers = list(type = c(1)))
  filtered_obj_spatraster <- ea.subset.spatial(obj_spatraster, "type", 99)
  expect_true(all(is.na(terra::values(filtered_obj_spatraster@data$value))))
})

test_that("ea.subset.spatial throws error for non-ea_spatial object", {
  if (requireNamespace("sf", quietly = TRUE)) {
    df <- sf::st_sf(data.frame(val = 1), geometry = sf::st_sfc(sf::st_point(c(0,0)), crs=4326))
    expect_error(
      ea.subset.spatial(df, "val", 1),
      "x must be an ea_spatial object.",
      fixed = TRUE
    )
  }
})

test_that("ea.subset.spatial throws error for non-existent column/layer", {
  obj <- create_test_sf()
  expect_error(
    ea.subset.spatial(obj, "non_existent_col", 1),
    "Column/layer non_existent_col not found in data.",
    fixed = TRUE
  )
})

# --- Validity Method Tests ---

test_that("ea_spatial validity method works correctly for supported types", {
  expect_no_error(create_test_sf())
  expect_no_error(create_test_stars())
  expect_no_error(create_test_spatraster())
  
  # Test valid object via new()
  if (requireNamespace("sf", quietly = TRUE)) {
    valid_sf_data <- sf::st_sf(data.frame(value = 1), geometry = sf::st_sfc(sf::st_point(c(0,0)), crs=4326))
    valid_obj_new <- methods::new("ea_spatial", meta = list(), data = valid_sf_data)
    expect_true(methods::validObject(valid_obj_new))
  }
})

test_that("ea_spatial validity method identifies missing 'value' layer", {
  if (requireNamespace("sf", quietly = TRUE)) {
    sf_no_val <- sf::st_sf(data.frame(x = 1), geometry = sf::st_sfc(sf::st_point(c(0,0)), crs=4326))
    expect_error(
      invalid_obj <- methods::new("ea_spatial", meta = list(), data = sf_no_val)
      ,
      "The 'data' object must contain a 'value' column or layer.",
      fixed = TRUE
    )
  }
})

test_that("ea_spatial validity method identifies unsupported data class", {
  expect_error(
    invalid_obj <- methods::new("ea_spatial", meta = list(), data = data.frame(value = 1))
    ,
    "The 'data' slot must be an object of class `sf`, `stars`, or `SpatRaster`.",
    fixed = TRUE
  )
})
