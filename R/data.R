##' ONI -- Oceanographic Niño Index
##'
##' The Oceanic Niño Index is a monthly index which is one measure of the El
##' Niño-Southern Oscillation.
##'
##' The Oceanic Niño Index (ONI) is a 3-month running
##' mean of sea surface temperature (SST) anomalies in the Niño 3.4 region
##' (5 deg N to 5 deg S, 120 deg W to 170 deg W) plotted on
##' the center month. The SST anomalies are calculated based on 30-year base
##' periods that are updated every 5 years, which accounts for global warming
##' and some of the decadal-scale SST variability (as seen in the Pacific
##' Decadal Oscillation index).
##' The ONI is provided by the NOAA’s National Weather
##' Service National Centers for Environmental Prediction CPC:
##' http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
##'
##' Preceding paragraph adapted from: Ross, T., and Robert, M. (2022). Normal
##' temperatures despite strong cool
##' climate indices and an emerging freshening trend. Pages 23-30 of
##' Boldt, J.L., Joyce, E., Tucker, S., and Gauthier, S. (Eds.). 2022. State of
##' the physical, biological and selected fishery resources of Pacific Canadian
##' marine ecosystems in 2021. Canadian Technical Report of Fisheries and
##' Aquatic Sciences. 3482 vii+242 p.
##'
##' The above website is updated automatically on the first Thursday of each
##' month, and states that:
##'
##' Because of the high frequency filter applied to the
##' ERSSTv5 data, ONI values may change up to two months after
##' the initial "real time" value is posted. Therefore, the most recent ONI
##' values should be considered an estimate.
##' On the site, Warm (red) and cold (blue) periods are based on a threshold of
##' +/- 0.5 deg C for the Oceanic Niño Index (ONI) [3 month running mean of
##' ERSST.v5 SST anomalies in the Niño 3.4 region (5 deg N-5 deg S, 120 deg W
##' -170 deg W)], based on centered 30-year base periods updated every 5 years.
##' For historical purposes, periods of below and above normal SSTs are colored in
##' blue and red (on the website) when the threshold is met for a minimum of 5
##' consecutive overlapping seasons. The ONI is one measure of the El
##' Niño-Southern Oscillation, and other indices can confirm whether features
##' consistent with a coupled ocean-atmosphere phenomenon accompanied these periods.
##'
##' Also see https://www.ncei.noaa.gov/access/monitoring/enso/sst
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{month:}{month (1 to 12) of value}
##'   \item{val:}{absolute values of three-month averages (preceding, current, and
##'    next month), deg C; note that recent values may change in subsequent
##'   updates -- see details}
##'   \item{anom:}{anomalies based on 30-year base periods that are updated every
##'   5 years, deg C; note that recent values may change in subsequent
##'   updates -- see details}
##'  }
##' @examples
##' \dontrun{
##' oni
##' plot(oni)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"oni"

##' Estimated abundance of Grey Seals on the Scotian Shelf
##'
##' TODO The Pacific Harbour Seal (\emph{Phoca vitulina richardsi}) is the most abundant pinniped species in the
##' Northeast Pacific and is found throughout coastal and estuarine waters of
##' British Columbia. A coastwide survey of harbour seals was conducted from
##' 2015 to 2019 and used to inform the estimated trends in abundance given here.
##'
##' TODO The Pacific Harbour Seal stock in Canada is
##' assessed as a single stock. The coastwide abundance of harbour
##' seals in British Columbia was assessed through aerial surveys conducted over multiple
##' years, and corrected for area covered, survey timing relative to peak
##' pupping, and proportion of seals hauled out during surveys. Estimated
##' abundance was calculated for seven regions: the Strait of Georgia (SOG),
##' West Coast Vancouver Island (WCVI), Queen Charlotte Strait (QCS), Discovery
##' Passage (DP), Central Mainland Coast (CMC), Northern Mainland Coast (NMC),
##' and Haida Gwaii (HG); see Figure 1 of DFO (2022) for a map showing the
##' regions. The coastwide estimates are also given (labelled `Coastwide` so the
##' plot title is capitalised), for which the means are calculated as the sum of
##' the regions' means and the `low` and `high` values as TODO ???
##'
##'
##' TODO The estimated abundances
##' were calculated using Generalised Additive Models (GAMs), and are included here
##' (reproducing the trends shown in Figure 3 of DFO, 2022). See that figure to see
##' the amount of data available in each region; these data can be added to
##' `pacea` if desired -- please let us know. The final year of data in each
##' region is saved in the object `harbour_seals_data_final_year`.
##'
##' Historical estimates of abundance will change from stock assessment to stock
##' assessment, and so we use `grey_seals` to represent
##' the results from the most recent assessment, and also save
##' `grey_seals_2021` for the results from
##' the 2021. This is so that you can always refer to a specific set of
##' assessment results (rather than have your analyses change because you have
##' updated `marea` and we have replaced `grey_seals` with results from a
##' new assessment).
##'
##' For further details see Hammill et al. (2023), TODO from which some of the above text was
##' adapted.
##'
##' Hammill, M.O., S.P. Rossi, A. Mosnier, C.E. den Heyer,  W.D. Bowen and
##' G.B. Stenson. 2023.  Grey Seal Abundance and Harvest Advice in Canadian
##' Waters. DFO Can. Sci. Advis. Sec. Res. Doc. 2023/053. vi + 40 p.
##'
##' @format A tibble also of class `marea_grey_seals` with columns:
##' \describe{
##'   \item{year:}{Year of the estimate}
##'   \item{low:}{low end of the estimate of abundance, defined as
##'    the lower value of the 95\% credible interval}
##'   \item{median:}{median estimate of abundance, numbers of seals}
##'   \item{high:}{high end of the estimate of abundance, defined as
##'    the higher value of the 95\% credible interval}
##'  }
##'
##' @examples
##' \dontrun{
##' grey_seals
##' plot(grey_seals)
##' }
##' @author Andrew Edwards and Nell den Heyer
##' @source Estimates provided by Nell den Heyer, then wrangled and imported using
##'   `data-raw/grey-seals/grey-seals/grey-seals.R`.
"grey_seals"

##' @rdname grey_seals
"grey_seals_2021"

##' NAO -- North Atlantic Oscillation
##'
##' TODOThe Oceanic Niño Index is a monthly index which is one measure of the El
##' Niño-Southern Oscillation.
##'
##' TODO The Oceanic Niño Index (ONI) is a 3-month running
##' mean of sea surface temperature (SST) anomalies in the Niño 3.4 region
##' (5 deg N to 5 deg S, 120 deg W to 170 deg W) plotted on
##' the center month. The SST anomalies are calculated based on 30-year base
##' periods that are updated every 5 years, which accounts for global warming
##' and some of the decadal-scale SST variability (as seen in the Pacific
##' Decadal Oscillation index).
##' The ONI is provided by the NOAA’s National Weather
##' Service National Centers for Environmental Prediction CPC:
##' http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{anomaly:}{TODOanomalies based on .....}
##'  }
##' @examples
##' \dontrun{
##' nao
##' plot(nao)
##' }
##' @author Everyone
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"nao"



##' NAO -- AZMP Scotian Shelf Bottom Temperature
##'
##' Averaged water temperatures at sea floor, over NAFO areas 4X, 4V and 4W
##'
##' TODO The Oceanic Niño Index (ONI) is a 3-month running
##' mean of sea surface temperature (SST) anomalies in the Niño 3.4 region
##' (5 deg N to 5 deg S, 120 deg W to 170 deg W) plotted on
##' the center month. The SST anomalies are calculated based on 30-year base
##' periods that are updated every 5 years, which accounts for global warming
##' and some of the decadal-scale SST variability (as seen in the Pacific
##' Decadal Oscillation index).
##' The ONI is provided by the NOAA’s National Weather
##' Service National Centers for Environmental Prediction CPC:
##' http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{region:}{TODOanomalies based on .....}
##'    \item{value:}{temperature value}
##'  }
##' @examples
##' \dontrun{
##' azmp_bottom_temperature
##' plot(azmp_bottom_temperature)
##' }
##' @author Jaimie Harbin and Benoit Casault
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"azmp_bottom_temperature"

##' @title food habits
##' @description Metadata for food habits.
##' @format A dataframe with columns:
##' \describe{
##'   \item{DATASOURCE}{}
##'   \item{MISSION}{Trip identifier}
##'   \item{SETNO}{Set number}
##'   \item{SDATE}{Set date}
##'   \item{STIME}{Set time (24 hr)}
##'   \item{STRAT}{Stratum}
##'   \item{BOTTOM_TEMPERATURE}{Water temperature in °C}
##'   \item{DEPTH}{Bottom depth in meters}
##'   \item{GEAR}{
##'  \describe{
#'   \item{3}{Yankee #36 otter trawl}
#'   \item{4}{#41.5 otter trawl}
#'   \item{5}{Longline}
#'   \item{7}{Midwater trawl}
#'   \item{9}{Western IIA trawl}
#'   \item{11}{Recreational angling}
#'   \item{14}{Campelen trawl}
#'   \item{15}{Nephrops trawl}
#'   \item{16}{NEST trawl}
#'   \item{17}{Balloon 280 trawl}
#'   \item{18}{Balloon 300 trawl}
#'   }
#'   }
##'   \item{SLATDD}{Set latitude (DD.DDDDDD)}
##'   \item{SLONGDD}{Set longitude (-DDD.DDDDDD)}
##'   \item{NAFO_ZONE}{NAFO zone level identification}
##'   \item{NAFO_SUBUNIT}{NAFO subunit level of identification}
##'   \item{SPEC}{see attached SDSPEC list for codes}
##'   \item{FSHNO}{Individual fish number}
##'   \item{FWT}{Fish length in cm}
##'   \item{FLEN}{Fish weight in grams}
##'   \item{STOWGT}{Total full stomach (with contents) weight in grams }
##'   \item{EMPTYWGT}{Empty stomach (without contents) weights in grams}
##'   \item{FULLNESS}{
##'   \describe{
##'   \item{0}{empty - no food contents}
##'   \item{1}{less than ¼ full}
##'   \item{2}{¼ to ½ full}
##'   \item{3}{½ to ¾ full}
##'   \item{4}{¾ full to full}
##'   \item{5}{everted}
##'   \item{6}{regurgitated}
##'   }}
##'   \item{FGEN}{
##'   \describe{
##'   \item{0}{Unknown}
##'   \item{1}{Male}
##'   \item{2}{Female}
##'   }}
##'   \item{PREYSPECCD}{}
##'   \item{PWT}{Prey weight in grams}
##'   \item{PLEN}{Prey length in cm}
##'   \item{PNUM}{Number of prey}
##'   \item{DIGESTION}{
##' \describe{
##'   \item{1}{Good Condition}
##'   \item{2}{Partly Digested}
##'   \item{3}{Well Digested}
##'   \item{4}{Unidentifiable}
##'   \item{9}{Digestion state not recorded}
##'   }}
##'  }
##' @author Manon Cassista Da Ros; Mike McMahon
##' @source Generated from running `data-raw/ecosystem-survey/food_habits.R`.
"food_habits"

