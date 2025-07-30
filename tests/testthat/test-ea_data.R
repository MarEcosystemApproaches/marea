library(testthat)
library(dplyr) # For tibble and rename in the constructor logic
library(tibble) # For as_tibble in the constructor logic

# Helper function to create a basic ea_data object for tests
create_test_ea_data <- function(years = 2000:2005, values = rnorm(6),
                                value_col_name = "temp_c",
                                data_type = "temperature",
                                region = "Scotian Shelf",
                                location_descriptor = "bottom",
                                units = "°C",
                                extra_cols = NULL) {
  df <- data.frame(year = years)
  df[[value_col_name]] <- values
  if (!is.null(extra_cols)) {
    for (col_name in names(extra_cols)) {
      df[[col_name]] <- extra_cols[[col_name]]
    }
  }
  
  ea_data(
    data = df,
    value_col = value_col_name,
    data_type = data_type,
    region = region,
    location_descriptor = location_descriptor,
    units = units
  )
}

test_that("ea_data S4 class definition is correct", {
  # This tests the internal structure of the class
  obj <- new("ea_data", meta = list(a = 1), data = data.frame(year = 1, value = 1))
  expect_s4_class(obj, "ea_data")
  expect_true(is.list(obj@meta))
  expect_true(is.data.frame(obj@data))
  expect_named(obj@meta)
  expect_named(obj@data)
})


test_that("ea_data constructor creates valid object with required slots", {
  obj <- create_test_ea_data()
  
  expect_s4_class(obj, "ea_data")
  expect_true(all(c("meta", "data") %in% slotNames(obj)))
  
  # Check meta slot
  expect_true(is.list(obj@meta))
  expect_true(all(c("data_type", "region", "location_descriptor", "units",
                    "species", "source_citation", "original_value_col") %in% names(obj@meta)))
  expect_equal(obj@meta$data_type, "temperature")
  expect_equal(obj@meta$region, "Scotian Shelf")
  expect_equal(obj@meta$original_value_col, "temp_c")
  expect_equal(obj@meta$species, NA_character_)
  expect_equal(obj@meta$source_citation, "No citation provided")
  
  # Check data slot
  expect_s3_class(obj@data, c("tbl_df", "tbl", "data.frame")) # Should be a tibble
  expect_true(all(c("year", "value") %in% names(obj@data)))
  expect_equal(colnames(obj@data), c("year", "value")) # Only year and value if no extra cols
  expect_equal(obj@data$year, 2000:2005)
  expect_length(obj@data$value, 6)
})

test_that("ea_data constructor handles optional arguments and extra metadata", {
  df <- data.frame(year = 2000:2001, biomass = c(100, 110))
  obj <- ea_data(
    data = df,
    value_col = "biomass",
    data_type = "biomass",
    region = "Pacific",
    location_descriptor = "offshore",
    units = "kg",
    species = "Cod",
    source_citation = "Smith et al. 2020",
    project_id = "ABC",
    sampling_freq = "annual"
  )
  
  expect_equal(obj@meta$species, "Cod")
  expect_equal(obj@meta$source_citation, "Smith et al. 2020")
  expect_equal(obj@meta$project_id, "ABC")
  expect_equal(obj@meta$sampling_freq, "annual")
})

test_that("ea_data constructor throws errors for missing/invalid columns", {
  df_no_year <- data.frame(x = 1:5, temp_c = rnorm(5))
  expect_error(
    ea_data(df_no_year, "temp_c", "t", "r", "l", "u"),
    "`data` must contain a 'year' column.",
    fixed = TRUE
  )
  
  df_no_value_col <- data.frame(year = 1:5, x = rnorm(5))
  expect_error(
    ea_data(df_no_value_col, "temp_c", "t", "r", "l", "u"),
    "Column 'temp_c' not found in the data.",
    fixed = TRUE
  )
  
  df_non_numeric_year <- data.frame(year = letters[1:5], temp_c = rnorm(5))
  expect_error(
    ea_data(df_non_numeric_year, "temp_c", "t", "r", "l", "u"),
    "Columns 'year' and 'temp_c' must be numeric.",
    fixed = TRUE
  )
  
  df_non_numeric_value <- data.frame(year = 1:5, temp_c = letters[1:5])
  expect_error(
    ea_data(df_non_numeric_value, "temp_c", "t", "r", "l", "u"),
    "Columns 'year' and 'temp_c' must be numeric.",
    fixed = TRUE
  )
})

test_that("ea_data constructor correctly renames value column", {
  df <- data.frame(year = 2000:2001, my_value = c(10, 20))
  obj <- ea_data(
    data = df,
    value_col = "my_value",
    data_type = "t", region = "r", location_descriptor = "l", units = "u"
  )
  expect_true("value" %in% names(obj@data))
  expect_false("my_value" %in% names(obj@data))
  expect_equal(obj@data$value, c(10, 20))
  expect_equal(obj@meta$original_value_col, "my_value")
})

test_that("ea_data constructor accepts tibbles as input data", {
  df_tibble <- tibble::tibble(year = 2000:2001, temp_c = rnorm(2))
  obj <- ea_data(
    data = df_tibble,
    value_col = "temp_c",
    data_type = "t", region = "r", location_descriptor = "l", units = "u"
  )
  expect_s4_class(obj, "ea_data")
  expect_s3_class(obj@data, "tbl_df")
})

test_that("ea_data constructor handles empty data frames gracefully (within validity)", {
  # An empty data frame with required columns is technically valid,
  # but likely indicates an issue downstream. The constructor should still work.
  df_empty <- data.frame(year = numeric(0), temp_c = numeric(0))
  obj <- ea_data(
    data = df_empty,
    value_col = "temp_c",
    data_type = "t", region = "r", location_descriptor = "l", units = "u"
  )
  expect_s4_class(obj, "ea_data")
  expect_equal(nrow(obj@data), 0)
  expect_true(all(c("year", "value") %in% names(obj@data)))
})


# --- `[[` Accessor Tests ---

test_that("[[ operator extracts 'meta' slot", {
  obj <- create_test_ea_data()
  meta_list <- obj[["meta"]]
  expect_type(meta_list, "list")
  expect_equal(meta_list$data_type, "temperature")
  expect_equal(meta_list$region, "Scotian Shelf")
})

test_that("[[ operator extracts 'data' slot", {
  obj <- create_test_ea_data()
  data_df <- obj[["data"]]
  expect_s3_class(data_df, "data.frame")
  expect_equal(data_df$year, 2000:2005)
  expect_true("value" %in% names(data_df))
})

test_that("[[ operator extracts metadata fields directly", {
  obj <- create_test_ea_data(data_type = "Salinity", region = "Bay of Fundy")
  expect_equal(obj[["data_type"]], "Salinity")
  expect_equal(obj[["region"]], "Bay of Fundy")
  expect_equal(obj[["original_value_col"]], "temp_c") # Default value
})

test_that("[[ operator extracts data columns directly", {
  obj <- create_test_ea_data()
  expect_equal(obj[["year"]], 2000:2005)
  expect_type(obj[["value"]], "double")
  expect_length(obj[["value"]], 6)
})

test_that("[[ operator throws error for non-existent elements", {
  obj <- create_test_ea_data()
  expect_error(
    obj[["non_existent_field"]],
    "Element 'non_existent_field' not found in 'meta' or 'data' slots",
    fixed = TRUE
  )
  expect_error(
    obj[["invalid_column"]],
    "Element 'invalid_column' not found in 'meta' or 'data' slots",
    fixed = TRUE
  )
})

test_that("[[ operator works with extra metadata fields", {
  df <- data.frame(year = 2000:2001, temp_c = rnorm(2))
  obj <- ea_data(
    data = df,
    value_col = "temp_c",
    data_type = "t", region = "r", location_descriptor = "l", units = "u",
    project_name = "Ocean Survey"
  )
  expect_equal(obj[["project_name"]], "Ocean Survey")
})


# --- `[` Subsetting Tests ---

test_that("[ operator subsets rows correctly (numeric index)", {
  obj <- create_test_ea_data(years = 1:10, values = 1:10)
  subset_obj <- obj[1:3, ]
  expect_s4_class(subset_obj, "ea_data")
  expect_equal(subset_obj@data$year, 1:3)
  expect_equal(subset_obj@data$value, 1:3)
  expect_equal(nrow(subset_obj@data), 3)
  expect_equal(subset_obj@meta, obj@meta) # Metadata should be preserved
})

test_that("[ operator subsets rows correctly (logical index)", {
  obj <- create_test_ea_data(years = 1:5, values = 1:5)
  subset_obj <- obj[c(TRUE, FALSE, TRUE, FALSE, TRUE), ]
  expect_s4_class(subset_obj, "ea_data")
  expect_equal(subset_obj@data$year, c(1, 3, 5))
  expect_equal(subset_obj@data$value, c(1, 3, 5))
  expect_equal(nrow(subset_obj@data), 3)
})

test_that("[ operator subsets rows correctly (negative index)", {
  obj <- create_test_ea_data(years = 1:5, values = 1:5)
  subset_obj <- obj[-c(2, 4), ]
  expect_s4_class(subset_obj, "ea_data")
  expect_equal(subset_obj@data$year, c(1, 3, 5))
  expect_equal(subset_obj@data$value, c(1, 3, 5))
  expect_equal(nrow(subset_obj@data), 3)
})

test_that("[ operator preserves other columns in data slot", {
  obj <- create_test_ea_data(
    years = 1:3, values = 1:3,
    extra_cols = list(site = c("A", "B", "C"))
  )
  subset_obj <- obj[1:2, ]
  expect_equal(subset_obj@data$site, c("A", "B"))
  expect_true("site" %in% names(subset_obj@data))
})

test_that("[ operator warns and ignores column subsetting (j)", {
  obj <- create_test_ea_data()
  expect_warning(
    subset_obj <- obj[i = 1:length(obj@data$year), "year"],
    "Column subsetting"
  )
  expect_equal(subset_obj@data, obj@data) # Should return the whole data frame
  expect_warning(subset_obj <- subset_obj[1:3, "year"],
                 "Column subsetting")
  expect_equal(subset_obj@data$year, obj@data$year[1:3])
  expect_equal(subset_obj@data$value, obj@data$value[1:3])
  expect_true(all(c("year", "value") %in% names(subset_obj@data))) # Still has both
})

test_that("[ operator warns and ignores drop argument", {
  obj <- create_test_ea_data()
  expect_warning(
    subset_obj <- obj[1, , drop = TRUE],
    "Column subsetting"
  ) # Note: the warning for `j` might still be triggered depending on internal dispatch
  expect_s4_class(subset_obj, "ea_data") # Should not drop to vector/data.frame
  expect_equal(nrow(subset_obj@data), 1)
})

test_that("[ operator handles subsetting to zero rows", {
  obj <- create_test_ea_data(years = 1:5, values = 1:5)
  subset_obj <- obj[numeric(0), ] # Subset to no rows
  expect_s4_class(subset_obj, "ea_data")
  expect_equal(nrow(subset_obj@data), 0)
  expect_equal(colnames(subset_obj@data), c("year", "value")) # Columns are preserved
  expect_equal(subset_obj@meta, obj@meta)
})

test_that("[ operator handles subsetting with out-of-bounds indices (standard R behavior)", {
  obj <- create_test_ea_data(years = 1:5, values = 1:5)
  subset_obj <- obj[c(1, 6, 2), ] # Index 6 is out of bounds
  expect_equal(nrow(subset_obj@data), 3)
  expect_equal(subset_obj@data$year, c(1, NA, 2))
  expect_true(is.na(subset_obj@data$year[2]))
})


# --- `ea.subset` Helper Function Tests ---

test_that("ea.subset filters correctly by a single value", {
  obj <- create_test_ea_data(
    years = c(2000, 2001, 2000, 2002),
    values = c(10, 12, 11, 15),
    extra_cols = list(site = c("A", "B", "A", "C"))
  )
  filtered_obj <- ea.subset(obj, "year", 2000)
  expect_s4_class(filtered_obj, "ea_data")
  expect_equal(filtered_obj@data$year, c(2000, 2000))
  expect_equal(filtered_obj@data$value, c(10, 11))
  expect_equal(filtered_obj@data$site, c("A", "A"))
  expect_equal(filtered_obj@meta, obj@meta) # Metadata preserved
})

test_that("ea.subset filters correctly by multiple values", {
  obj <- create_test_ea_data(
    years = c(2000, 2001, 2000, 2002, 2003),
    values = 1:5,
    extra_cols = list(site = c("A", "B", "A", "C", "B"))
  )
  filtered_obj <- ea.subset(obj, "site", c("A", "C"))
  expect_s4_class(filtered_obj, "ea_data")
  expect_equal(filtered_obj@data$site, c("A", "A", "C"))
  expect_equal(filtered_obj@data$year, c(2000, 2000, 2002))
  expect_equal(filtered_obj@data$value, c(1, 3, 4))
})

test_that("ea.subset returns empty object if no matches", {
  obj <- create_test_ea_data(years = 2000:2002, values = 1:3)
  filtered_obj <- ea.subset(obj, "year", 1999)
  expect_s4_class(filtered_obj, "ea_data")
  expect_equal(nrow(filtered_obj@data), 0)
  expect_equal(colnames(filtered_obj@data), c("year", "value")) # Columns retained
  expect_equal(filtered_obj@meta, obj@meta)
})

test_that("ea.subset throws error for non-ea_data object", {
  df <- data.frame(year = 1:5, value = 1:5)
  expect_error(
    ea.subset(df, "year", 2000),
    "x must be an ea_data object.",
    fixed = TRUE
  )
})

test_that("ea.subset throws error for non-existent column", {
  obj <- create_test_ea_data()
  expect_error(
    ea.subset(obj, "non_existent_col", 1),
    "Column non_existent_col not found in data.",
    fixed = TRUE
  )
})

test_that("ea.subset works with other column types (e.g., character)", {
  obj <- create_test_ea_data(
    years = 2000:2002, values = 1:3,
    extra_cols = list(season = c("spring", "summer", "autumn"))
  )
  filtered_obj <- ea.subset(obj, "season", "summer")
  expect_s4_class(filtered_obj, "ea_data")
  expect_equal(filtered_obj@data$year, 2001)
  expect_equal(filtered_obj@data$season, "summer")
})

test_that("ea.subset handles NA values in column gracefully", {
  obj <- create_test_ea_data(
    years = c(2000, 2001, 2002, 2003),
    values = 1:4,
    extra_cols = list(flag = c("A", NA, "B", "A"))
  )
  # Filter for non-NA
  filtered_obj_A <- ea.subset(obj, "flag", "A")
  expect_equal(filtered_obj_A@data$year, c(2000, 2003))
  
})


# --- Validity Method Tests (automatic checks by S4) ---

test_that("ea_data validity method works correctly", {
  # Valid object creation implicitly calls validity
  expect_no_error(create_test_ea_data())
  
  # Missing 'year'
  
  expect_error(
    invalid_obj_no_year <- new("ea_data",
                               meta = list(),
                               data = data.frame(value = 1:5)),
    "Missing 'year' column in the data slot.",
    fixed = TRUE
  )
  
  # Missing 'value'
  
  expect_error(
    invalid_obj_no_value <- new("ea_data",
                                meta = list(),
                                data = data.frame(year = 1:5)),
    "Missing 'value' column in the data slot.",
    fixed = TRUE
  )
  
  # Missing both
  
  expect_error(
    invalid_obj_no_both <- new("ea_data",
                               meta = list(),
                               data = data.frame(x = 1:5))
  )
  
  # Valid object via new()
  valid_obj_new <- new("ea_data",
                       meta = list(),
                       data = data.frame(year = 1, value = 1))
  expect_true(validObject(valid_obj_new))
})

# Test for handling different data types in original value column
test_that("ea_data constructor handles original_value_col renaming consistently", {
  df <- data.frame(year = 2000:2001, int_val = as.integer(c(10L, 20L)))
  obj <- ea_data(
    data = df,
    value_col = "int_val",
    data_type = "t", region = "r", location_descriptor = "l", units = "u"
  )
  expect_true("value" %in% names(obj@data))
  expect_false("int_val" %in% names(obj@data))
  expect_equal(obj@data$value, c(10, 20)) # Stored as double in tibble
  expect_equal(obj@meta$original_value_col, "int_val")
  
  df2 <- data.frame(year = 2000:2001, char_val = c("A", "B"))
  expect_error(
    ea_data(df2, "char_val", "t", "r", "l", "u"),
    "Columns 'year' and 'char_val' must be numeric."
  )
})

# Test `ea.subset` with factors
test_that("ea.subset works with factor columns", {
  df <- data.frame(year = 2000:2005, temp_c = rnorm(6),
                   site = factor(rep(c("A", "B"), each = 3)))
  obj <- ea_data(df, value_col = "temp_c", data_type = "temperature",
                 region = "Test Region", location_descriptor = "surface",
                 units = "°C")
  
  filtered_obj <- ea.subset(obj, "site", "A")
  expect_s4_class(filtered_obj, "ea_data")
  expect_equal(filtered_obj@data$site, factor(rep("A", 3), levels = c("A", "B")))
  expect_equal(nrow(filtered_obj@data), 3)
  
  # Filtering by a factor level not present in the data
  filtered_obj_C <- ea.subset(obj, "site", "C")
  expect_equal(nrow(filtered_obj_C@data), 0)
})
