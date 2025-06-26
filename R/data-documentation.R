##' Oceanic Niño Index (ONI)
##'
##' The Oceanic Niño Index (ONI) is a monthly measure of the El Niño-Southern Oscillation. It is calculated as a 3-month running mean of sea surface temperature (SST) anomalies in the Niño 3.4 region (5°N–5°S, 120°W–170°W), using 30-year base periods updated every 5 years. ONI values may change for up to two months after initial posting due to data filtering.
##'
##' For more information, see [NOAA ONI](http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml) and [NOAA ENSO SST](https://www.ncei.noaa.gov/access/monitoring/enso/sst).
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble of class `pacea_index` with columns:
##' \describe{
##'   \item{year}{Year of value}
##'   \item{month}{Month (1–12) of value}
##'   \item{val}{Three-month average SST (°C); recent values may change in subsequent updates}
##'   \item{anom}{SST anomaly (°C); recent values may change in subsequent updates}
##' }
##' @examples
##' \dontrun{
##' oni
##' plot(oni)
##' }
##' @docType data
##' @name oni
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
##' @references
##' Ross, T., & Robert, M. (2022). Normal temperatures despite strong cool climate indices and an emerging freshening trend. In Boldt, J.L. et al. (Eds.), State of the physical, biological and selected fishery resources of Pacific Canadian marine ecosystems in 2021. Canadian Technical Report of Fisheries and Aquatic Sciences, 3482, vii+242 p.
"oni"

# --- TODOs for developers ---
# TODO: Review and update ONI description if NOAA methodology changes.

##' Estimated Abundance of Grey Seals on the Scotian Shelf
##'
##' The grey seal population in Canadian waters is assessed as a single stock. Coastwide abundance estimates are based on aerial surveys conducted over multiple years, corrected for survey timing, area covered, and proportion of seals hauled out. Estimates are provided for seven regions and for the coastwide total.
##'
##' Abundance estimates use Generalised Additive Models (GAMs). The final year of data for each region is saved in `harbour_seals_data_final_year`.
##'
##' The `grey_seals` object represents the most recent assessment results. The `grey_seals_2021` object contains results from the 2021 assessment, allowing for reproducibility.
##'
##' For further details, see Hammill et al. (2023).
##'
##' @format A tibble of class `marea_grey_seals` with columns:
##' \describe{
##'   \item{year}{Year of the estimate}
##'   \item{low}{Lower 95\% credible interval}
##'   \item{median}{Median estimate of abundance}
##'   \item{high}{Upper 95\% credible interval}
##' }
##' @examples
##' \dontrun{
##' grey_seals
##' plot(grey_seals)
##' }
##' @docType data
##' @name grey_seals
##' @author Andrew Edwards and Nell den Heyer
##' @source Estimates provided by Nell den Heyer, processed using `data-raw/grey-seals/grey-seals/grey-seals.R`.
##' @references
##' Hammill, M.O., Rossi, S.P., Mosnier, A., den Heyer, C.E., Bowen, W.D., & Stenson, G.B. (2023). Grey Seal Abundance and Harvest Advice in Canadian Waters. DFO Can. Sci. Advis. Sec. Res. Doc. 2023/053. vi + 40 p.
"grey_seals"

##' @rdname grey_seals
"grey_seals_2021"

# --- TODOs for developers (not part of user documentation) ---
# TODO: Replace placeholder text about Pacific Harbour Seal with correct grey seal information.
# TODO: Clarify calculation of coastwide 'low' and 'high' values.
# TODO: Review and update text adapted from Hammill et al. (2023).

##' North Atlantic Oscillation (NAO) Index
##'
##' The NAO Index measures the strength of westerly winds over the North Atlantic, based on the difference in sea-level atmospheric pressures between the Azores and Iceland. It is calculated using a rotated principal component analysis of monthly standardized 500 mb height anomalies, averaged over December to March.
##'
##' Data are from the [NOAA NAO monitoring site](https://www.ncei.noaa.gov/access/monitoring/nao/).
##'
##' @format A tibble of class `pacea_index` with columns:
##' \describe{
##'   \item{year}{Year of value}
##'   \item{anomaly}{Index value (strength of westerly winds)}
##' }
##' @examples
##' \dontrun{
##' nao
##' plot(nao)
##' }
##' @docType data
##' @name nao
##' @author Jamie C. Tam; Chantelle Layton
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"nao"

# --- TODOs for developers (not part of user documentation) ---
# TODO: Review and clarify if additional NAO indices (e.g., seasonal) should be included.

##' AZMP Scotian Shelf Bottom Temperature
##'
##' Average water temperatures at the sea floor, over NAFO areas 4X, 4V, and 4W, as part of the Atlantic Zone Monitoring Program (AZMP).
##'
##' @format A tibble of class `pacea_index` with columns:
##' \describe{
##'   \item{year}{Year of value}
##'   \item{region}{Region identifier}
##'   \item{value}{Temperature value (°C)}
##' }
##' @examples
##' \dontrun{
##' azmp_bottom_temperature
##' plot(azmp_bottom_temperature)
##' }
##' @docType data
##' @name azmp_bottom_temperature
##' @author Jaimie Harbin and Benoit Casault
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"azmp_bottom_temperature"

# --- TODOs for developers (not part of user documentation) ---
# TODO: Complete region and anomaly descriptions in @format.

##' Food Habits Metadata
##'
##' Metadata for food habits data collected during ecosystem surveys.
##'
##' @format A data frame with columns:
##' \describe{
##'   \item{DATASOURCE}{Data source}
##'   \item{MISSION}{Trip identifier}
##'   \item{SETNO}{Set number}
##'   \item{SDATE}{Set date}
##'   \item{STIME}{Set time (24 hr)}
##'   \item{STRAT}{Stratum}
##'   \item{BOTTOM_TEMPERATURE}{Water temperature (°C)}
##'   \item{DEPTH}{Bottom depth (m)}
##'   \item{GEAR}{Gear type. Codes:
##'     \describe{
##'       \item{3}{Yankee #36 otter trawl}
##'       \item{4}{#41.5 otter trawl}
##'       \item{5}{Longline}
##'       \item{7}{Midwater trawl}
##'       \item{9}{Western IIA trawl}
##'       \item{11}{Recreational angling}
##'       \item{14}{Campelen trawl}
##'       \item{15}{Nephrops trawl}
##'       \item{16}{NEST trawl}
##'       \item{17}{Balloon 280 trawl}
##'       \item{18}{Balloon 300 trawl}
##'     }
##'   }
##'   \item{SLATDD}{Set latitude (decimal degrees)}
##'   \item{SLONGDD}{Set longitude (decimal degrees, negative for west)}
##'   \item{NAFO_ZONE}{NAFO zone}
##'   \item{NAFO_SUBUNIT}{NAFO subunit}
##'   \item{SPEC}{Species code (see SDSPEC list)}
##'   \item{FSHNO}{Individual fish number}
##'   \item{FWT}{Fish length (cm)}
##'   \item{FLEN}{Fish weight (g)}
##'   \item{STOWGT}{Full stomach weight (g)}
##'   \item{EMPTYWGT}{Empty stomach weight (g)}
##'   \item{FULLNESS}{Stomach fullness. Codes:
##'     \describe{
##'       \item{0}{Empty}
##'       \item{1}{< ¼ full}
##'       \item{2}{¼–½ full}
##'       \item{3}{½–¾ full}
##'       \item{4}{¾–full}
##'       \item{5}{Everted}
##'       \item{6}{Regurgitated}
##'     }
##'   }
##'   \item{FGEN}{Fish gender. Codes:
##'     \describe{
##'       \item{0}{Unknown}
##'       \item{1}{Male}
##'       \item{2}{Female}
##'     }
##'   }
##'   \item{PREYSPECCD}{Prey species code}
##'   \item{PWT}{Prey weight (g)}
##'   \item{PLEN}{Prey length (cm)}
##'   \item{PNUM}{Number of prey}
##'   \item{DIGESTION}{Digestion state. Codes:
##'     \describe{
##'       \item{1}{Good condition}
##'       \item{2}{Partly digested}
##'       \item{3}{Well digested}
##'       \item{4}{Unidentifiable}
##'       \item{9}{Not recorded}
##'     }
##'   }
##' }
##' @docType data
##' @name food_habits
##' @author Manon Cassista Da Ros; Mike McMahon
##' @source Generated from running `data-raw/ecosystem-survey/food_habits.R`.
"food_habits"

# --- TODOs for developers (not part of user documentation) ---
# TODO: Complete DATASOURCE and PREYSPECCD descriptions.
# TODO: Review and update SDSPEC list reference.
