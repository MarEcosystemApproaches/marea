# marea: Modernized coastwide index data constructor script
devtools::load_all()
library(dplyr)
library(readr)
library(tibble)
library(marea)

# Helper function
make_ea_index <- function(
    df, value_col, data_type, region, location, units, source, species = NA_character_
) {
  ea_data(
    data = df,
    value_col = value_col,
    data_type = data_type,
    region = region,
    location_descriptor = location,
    units = units,
    species = species,
    source_citation = source
  )
}

# ---- NAO ----
remotes::install_github("casaultb/azmpdata")
library(azmpdata)

nao_new <- tibble(
  year = azmpdata::Derived_Annual_Broadscale$year,
  anomaly = azmpdata::Derived_Annual_Broadscale$north_atlantic_oscillation
) %>%
  filter(!is.na(anomaly))

nao <- make_ea_index(
  df = nao_new,
  value_col = "anomaly",
  data_type = "North Atlantic Oscillation Index",
  region = "North Atlantic",
  location = "Azores–Iceland SLP difference",
  units = "",  # Standardized index
  source = "NOAA NCEP via azmpdata; https://www.ncei.noaa.gov/access/monitoring/nao/"
)
usethis::use_data(nao, overwrite = TRUE)

# ---- AZMP BOTTOM TEMPERATURE ----
azmp_bottom_temperature_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$temperature_at_sea_floor
) %>%
  filter(!is.na(mean))

azmp_bottom_temperature <- make_ea_index(
  df = azmp_bottom_temperature_new,
  value_col = "mean",
  data_type = "Bottom Temperature",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea floor mean temperature",
  units = "deg C",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata"
)
usethis::use_data(azmp_bottom_temperature, overwrite = TRUE)

# ---- ONI ----
download.file("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",
              destfile = "oni.txt", mode = "wb", quiet = FALSE)
oni_new <- read_table("oni.txt")
stopifnot(colnames(oni_new) == c("SEAS", "YR", "TOTAL", "ANOM"),
          oni_new[1, 1] == "DJF")
colnames(oni_new) <- c("month", "year", "value", "anomaly")
oni_new <- relocate(oni_new, year)
oni_new$month <- as.numeric(
  factor(oni_new$month, levels = unique(oni_new$month), ordered = TRUE)
)
oni <- make_ea_index(
  df = oni_new,
  value_col = "value",
  data_type = "Oceanic Niño Index",
  region = "Niño 3.4 Region (Pacific)",
  location = "5°N–5°S, 120°W–170°W",
  units = "deg C anomaly",
  source = "NOAA CPC, https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml"
)
usethis::use_data(oni, overwrite = TRUE)

# ---- PDO ----
download.file("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
              destfile = "pdo.txt", mode = "wb", quiet = FALSE)
pdo_raw <- read_table("pdo.txt", skip = 1, na = "99.99")
stopifnot(pdo_raw[1,1] == 1854)
pdo_long <- tidyr::pivot_longer(pdo_raw, cols = "Jan":"Dec",
                                names_to = "month", values_to = "anomaly") %>%
  mutate(month = as.numeric(match(month, month.abb))) %>%
  rename(year = Year) %>%
  filter(!is.na(anomaly))
pdo <- make_ea_index(
  df = pdo_long,
  value_col = "anomaly",
  data_type = "Pacific Decadal Oscillation",
  region = "North Pacific",
  location = "PDO index area",
  units = "", # Standardized
  source = "NOAA ERSST, https://www.ncei.noaa.gov/access/monitoring/pdo/"
)
usethis::use_data(pdo, overwrite = TRUE)

# ---- SOI ----
download.file("https://www.cpc.ncep.noaa.gov/data/indices/soi",
              destfile = "soi.txt", mode = "wb", quiet = FALSE)
soi_new <- read.table("soi.txt", skip = 3, as.is = TRUE, header = TRUE, fill = TRUE)
names(soi_new)[1] <- "year"
soi_new$year <- as.numeric(soi_new$year)
soi_new <- soi_new[1:(min(which(is.na(soi_new$year))) - 1), ]
soi_new[nrow(soi_new), ] <- stringr::str_replace_all(soi_new[nrow(soi_new), ], "-999.9", "")
soi_new <- dplyr::mutate_all(soi_new, as.numeric)
soi_long <- tidyr::pivot_longer(soi_new, cols = "JAN":"DEC", names_to = "month", values_to = "anomaly") %>%
  mutate(month = as.numeric(match(month, toupper(month.abb)))) %>%
  filter(!is.na(anomaly))
soi <- make_ea_index(
  df = soi_long,
  value_col = "anomaly",
  data_type = "Southern Oscillation Index",
  region = "Equatorial Pacific",
  location = "Tahiti-Darwin (SLP diff)",
  units = "", # index
  source = "NOAA CPC, https://www.cpc.ncep.noaa.gov/data/indices/soi"
)
usethis::use_data(soi, overwrite = TRUE)

# ---- NPGO ----
download.file("http://www.o3d.org/npgo/data/NPGO.txt",
              destfile = "npgo.txt", mode = "wb", quiet = FALSE)
npgo_raw <- read.table("npgo.txt", skip = 5, as.is = TRUE, header = FALSE, fill = TRUE, comment = "#") %>%
  as_tibble() %>%
  rename(year = V1, month = V2, anomaly = V3) %>%
  filter(!is.na(month)) %>%
  mutate(year = as.numeric(year))
npgo <- make_ea_index(
  df = npgo_raw,
  value_col = "anomaly",
  data_type = "North Pacific Gyre Oscillation",
  region = "North Pacific Gyre",
  location = "NPGO",
  units = "normalized anomaly",
  source = "Di Lorenzo et al., http://www.o3d.org/npgo/"
)
usethis::use_data(npgo, overwrite = TRUE)

# ---- MEI ----
download.file("https://psl.noaa.gov/enso/mei/data/meiv2.data",
              destfile = "mei.txt", mode = "wb", quiet = FALSE)
mei_new <- read.table("mei.txt", skip = 1, as.is = TRUE, fill = TRUE) %>% as_tibble()
names(mei_new) <- c("year", 1:12)
mei_new$year <- as.numeric(mei_new$year)
mei_new <- mei_new[seq_len(which(mei_new$year == -999)[1] - 1), ]
mei_long <- tidyr::pivot_longer(mei_new, cols = "1":"12", names_to = "month", values_to = "anomaly") %>%
  mutate(month = as.numeric(month), anomaly = as.numeric(anomaly)) %>%
  filter(anomaly != -999)
mei <- make_ea_index(
  df = mei_long,
  value_col = "anomaly",
  data_type = "Multivariate ENSO Index (Version 2)",
  region = "Equatorial Pacific",
  location = "MEI composite",
  units = "standardized anomaly",
  source = "NOAA ESRL/PSL, https://psl.noaa.gov/enso/mei/"
)
usethis::use_data(mei, overwrite = TRUE)

# ---- AO ----
download.file("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table",
              destfile = "ao.txt", mode = "wb", quiet = FALSE)
ao_raw <- read.table("ao.txt", header = TRUE, fill = TRUE)
ao_years <- as.numeric(row.names(ao_raw))
ao_long <- cbind(year = ao_years, ao_raw) %>%
  as_tibble()
row.names(ao_long) <- NULL
ao_long2 <- tidyr::pivot_longer(ao_long, cols = "Jan":"Dec", names_to = "month", values_to = "anomaly") %>%
  mutate(month = as.numeric(match(month, month.abb))) %>%
  filter(!is.na(anomaly))
ao <- make_ea_index(
  df = ao_long2,
  value_col = "anomaly",
  data_type = "Arctic Oscillation Index",
  region = "Northern Hemisphere",
  location = "AO PCA",
  units = "",
  source = "NOAA CPC, https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/"
)
usethis::use_data(ao, overwrite = TRUE)

# ---- Add more indices (npi_monthly, npi_annual, bi, etc.) as needed using this pattern ----

# ---- END ----
message("All coastwide indices updated using ea_data class and with full metadata.")

