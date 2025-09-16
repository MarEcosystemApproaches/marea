#' Oceanic Niño Index (ONI)
#'
#' The Oceanic Niño Index (ONI) is a monthly measure of the El Niño-Southern Oscillation. It is calculated as a 3-month running mean of sea surface temperature (SST) anomalies in the Niño 3.4 region (5°N–5°S, 120°W–170°W), using 30-year base periods updated every 5 years. ONI values may change for up to two months after initial posting due to data filtering.
#'
#' For more information, see \href{http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml}{NOAA ONI} and \href{https://www.ncei.noaa.gov/access/monitoring/enso/sst}{NOAA ENSO SST}.
#'
#' @format An `ea_data` object with:
#' \describe{
#'   \item{data}{A tibble with columns:
#'     \describe{
#'       \item{year}{Year (numeric)}
#'       \item{month}{Month (numeric, 1–12)}
#'       \item{value}{Three-month average sea surface temperature (SST, °C)}
#'       \item{anomaly}{SST anomaly (°C)}
#'     }
#'   }
#'   \item{meta}{A named list with:
#'     \describe{
#'       \item{data_type}{Character. "Oceanic Niño Index (ONI)"}
#'       \item{region}{Character. "Pacific"}
#'       \item{location_descriptor}{Character. "Niño 3.4 region"}
#'       \item{units}{Character. "°C"}
#'       \item{species}{Character. \code{NA_character_} (not applicable)}
#'       \item{source_citation}{Character. NOAA CPC, see links above}
#'       \item{original_value_col}{Character. Name of value source column, e.g. "val"}
#'     }
#'   }
#' }
#' @name oni
#' @docType data
#' @author Andrew Edwards
#' @source Generated in data-raw/coastwide-indices/coastwide-indices.R
#' @references Ross, T. & Robert, M. (2022). In Boldt, J.L. et al. Canadian Technical Report of Fisheries and Aquatic Sciences, 3482.
"oni"

#' Estimated Grey Seal Abundance (ea_data)
#'
#' Abundance of grey seals in Canadian waters, assessed as a single stock. Derived from aerial surveys, corrected for survey timing, coverage, and haul-out proportion.
#'
#' @format An `ea_data` object with:
#' \describe{
#'    \item{data}{A tibble:
#'     \describe{
#'       \item{year}{Year (numeric)}
#'       \item{value}{Median estimate (count)}
#'       \item{low}{Lower 95percent credible interval}
#'       \item{high}{Upper 95percent credible interval}
#'     }
#'   }
#'   \item{meta}{A named list with:
#'     \describe{
#'       \item{data_type}{"Grey Seal Abundance"}
#'       \item{region}{ "Scotian Shelf"}
#'       \item{location_descriptor}{"Sable Island / Scotian Shelf"}
#'       \item{units}{"count"}
#'       \item{species}{"Halichoerus grypus"}
#'       \item{source_citation}{Hammill et al. (2023)}
#'       \item{original_value_col}{Character. "median"}
#'     }
#'   }
#' }
#' 
#' @name grey_seals
#' @docType data
#' @author Andrew Edwards, Nell den Heyer
#' @source Hammill et al. (2023), see data-raw/grey-seals/grey-seals.R
"grey_seals"

#' @rdname grey_seals
"grey_seals_2021"

#' North Atlantic Oscillation (NAO) Index (ea_data)
#'
#' The NAO Index represents the difference in sea-level atmospheric pressures between the Azores and Iceland, calculated by PCA of 500 mb height anomalies.
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{A tibble: \describe{
#'     \item{year}{Year}
#'     \item{value}{NAO anomaly value}
#'   }}
#'   \item{meta}{A named list. Keys include:
#'     \itemize{
#'       \item{\code{data_type}:} "NAO Index"
#'       \item{\code{region}:} "North Atlantic"
#'       \item{\code{location_descriptor}:} "Azores-Iceland"
#'       \item{\code{units}:} "index"
#'       \item{\code{species}:} \code{NA}
#'       \item{\code{source_citation}:} "NOAA, see source"
#'       \item{\code{original_value_col}:} e.g. "anomaly"
#'     }
#'   }
#' }
#' @name nao
#' @docType data
#' @author Jamie C. Tam; Chantelle Layton
#' @source \url{https://www.ncei.noaa.gov/access/monitoring/nao/}
"nao"

#' GLORYS Bottom Temperature (ea_spatial)
#'
#' Monthly bottom temperature fields from the GLORYS reanalysis, provided as a spatial object for the Maritimes region.
#' The latest ten years of data are included from the Copernicus Marine Service (CMEMS) GLORYS product (monthly and 
#' interim monthly) for best data coverage.
#'
#' Dataset IDs: cmems_mod_glo_phy_my_0.083deg_P1M-m and cmems_mod_glo_phy_myint_0.083deg_P1M-m
#' Pulled using marea::get_CMEMS_ncdf()
#' 
#' @format An `ea_spatial` (sf) object:
#' \describe{
#'   \item{value}{Bottom temperature value (°C)}
#'   \item{month}{Month of observation}
#'   \item{geometry}{Spatial geometry (sf object)}
#' }
#' \strong{Metadata (\code{attr(obj, "meta")})}:
#' \describe{
#'   \item{data_type}{Character. "GLORYS Bottom Temperature"}
#'   \item{region}{Character. "Maritimes"}
#'   \item{time_descriptor}{Character. Month, year, or period}
#'   \item{units}{Character. "°C"}
#'   \item{source_citation}{Character. "Copernicus Marine Service"}
#'   \item{original_value_col}{Character. The original col used for value}
#' }
#' @name glorys_bottom_temperature
#' @docType data
#' @author marea/DFO
#' @source Copernicus Marine Service (GLORYS), see data-raw/glorys/glorys-bottom-temperature.R
"glorys_bottom_temperature"

#' AZMP Scotian Shelf Bottom Temperature (ea_data)
#'
#' Bottom water temperature (annual mean) from the Atlantic Zone Monitoring Program (AZMP) for the Scotian Shelf, covering NAFO Divisions 4X, 4V, and 4W. Data are averaged from in situ surveys and are crucial for fisheries and climate analyses.
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{A tibble with columns:
#'     \itemize{
#'       \item{\code{year}:} Numeric year
#'       \item{\code{region}:} Character NAFO division or area name
#'       \item{\code{value}:} Numeric temperature at sea floor (°C)
#'     }
#'   }
#'   \item{meta}{A named list, including:
#'     \itemize{
#'       \item{\code{data_type}:} "Annual Bottom Temperature"
#'       \item{\code{region}:} "Scotian Shelf"
#'       \item{\code{location_descriptor}:} "NAFO 4X-4W"
#'       \item{\code{units}:} "°C"
#'       \item{\code{species}:} \code{NA}
#'       \item{\code{source_citation}:} "DFO AZMP, https://www.bio.gc.ca/science/monitoring-monitorage/azmp-pmza/overview-eng.php"
#'       \item{\code{original_value_col}:} "value"
#'     }
#'   }
#' }
#' @docType data
#' @name azmp_bottom_temperature
#' @author Jaimie Harbin and Benoit Casault
#' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`
"azmp_bottom_temperature"

#' Food Habits 
#'
#' Stomach content and food habits data collected during DFO ecosystem surveys in the Maritimes region. Contains trawl set, fish, and prey item information.
#'
#'@format An `ea_data` object:
#' \describe{
#'  \item{data}{Tibble:
#'    \describe{
#'      \item{SET_SEQ_value}{Sequence identifier for the sampling set}
#'      \item{PRED_SEQ_value}{Sequence identifier for predator}
#'      \item{PREY_SEQ_value}{Sequence identifier for prey}
#'      \item{DATASOURCE_value}{Data source code}
#'      \item{MISSION_value}{Survey mission code}
#'      \item{SETNO_value}{Set number within the mission}
#'      \item{SDATE_value}{Set date (format: DD-Mmm-YY)}
#'      \item{STIME_value}{Set time (HHMM)}
#'      \item{STRAT_value}{Stratum identifier}
#'      \item{BOTTOM_TEMPERATURE_value}{Bottom water temperature (degrees Celsius)}
#'      \item{DEPTH_value}{Bottom depth (meters)}
#'      \item{GEAR_value}{Gear code used for sampling}
#'      \item{SLATDD_value}{Set latitude (decimal degrees)}
#'      \item{SLONGDD_value}{Set longitude (decimal degrees)}
#'      \item{region}{Survey region code (e.g., NAFO division)}
#'      \item{NAFO_SUBUNIT_value}{NAFO subunit code}
#'      \item{SPEC_value}{Predator species code}
#'      \item{FSHNO_value}{Predator fish specimen number}
#'      \item{FWT_value}{Predator whole weight (grams)}
#'      \item{FLEN_value}{Predator length (cm)}
#'      \item{STOWGT_value}{Predator stomach total weight (grams)}
#'      \item{EMPTYWGT_value}{Predator stomach empty weight (grams)}
#'      \item{FULLNESS_value}{Predator stomach fullness index}
#'      \item{FGEN_value}{Predator gender code}
#'      \item{PREYSPECCD_value}{Prey species code}
#'      \item{PWT_value}{Prey whole weight (grams)}
#'      \item{PLEN_value}{Prey length (cm)}
#'      \item{PNUM_value}{Prey count in stomach}
#'      \item{DIGESTION_value}{Prey digestion stage code}
#'      \item{year}{Year of the observation}
#'     }
#'   }
#'   \item{meta}{List includes:
#'     \itemize{
#'       \item{\code{data_type}:} "biological"
#'       \item{\code{region}:} "NAFO"
#'       \item{\code{location_descriptor}:} "Maritimes Region"
#'       \item{\code{units}:} "weights"
#'       \item{\code{source_citation}:} "Cook and Bundy 2010"
#'     }
#'   }
#' }
#' 
#' @docType data
#' @name food_habits
#' @author Manon Cassista-Da-Ros; Mike McMahon; Jamie C. Tam
#' @source Generated from running `data-raw/food-habits/food_habits.R`
"food_habits"

#' Pacific Decadal Oscillation (PDO) Index (ea_data)
#'
#' The PDO Index is a monthly index describing long-term sea surface temperature variability in the North Pacific (poleward of 20°N).
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{Tibble with:
#'     \itemize{
#'       \item{\code{year}:} Year
#'       \item{\code{month}:} Month (1–12)
#'       \item{\code{value}:} PDO anomaly (unitless)
#'     }
#'   }
#'   \item{meta}{List includes:
#'     \itemize{
#'       \item{\code{data_type}:} "Pacific Decadal Oscillation Index"
#'       \item{\code{region}:} "North Pacific"
#'       \item{\code{location_descriptor}:} "poleward 20°N North Pacific"
#'       \item{\code{units}:} ""
#'       \item{\code{source_citation}:} "NOAA NCEI; see data-raw"
#'       \item{\code{original_value_col}:} "anomaly"
#'     }
#'   }
#' }
#' @docType data
#' @name pdo
#' @author Andrew Edwards
#' @source \url{https://www.ncei.noaa.gov/access/monitoring/pdo/}
"pdo"

#' Southern Oscillation Index (SOI) (ea_data)
#'
#' The SOI is a monthly measure of the seasonal atmospheric pressure differences between Tahiti and Darwin, a leading indicator of El Niño/La Niña conditions.
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{Tibble with:
#'     \itemize{
#'       \item{\code{year}:} Year
#'       \item{\code{month}:} Month (1–12)
#'       \item{\code{value}:} SOI value (unitless index)
#'     }
#'   }
#'   \item{meta}{Metadata includes
#'     \itemize{
#'       \item{\code{data_type}:} "Southern Oscillation Index"
#'       \item{\code{region}:} "South Pacific"
#'       \item{\code{location_descriptor}:} "Tahiti-Darwin"
#'       \item{\code{units}:} "index"
#'       \item{\code{source_citation}:} "NOAA CPC"
#'       \item{\code{original_value_col}:} "anomaly"
#'     }
#'   }
#' }
#' @docType data
#' @name soi
#' @author Andrew Edwards
#' @source \url{https://www.cpc.ncep.noaa.gov/data/indices/soi}
"soi"

#' North Pacific Gyre Oscillation (NPGO) Index (ea_data)
#'
#' The NPGO is the second dominant mode of sea-surface height anomaly variability in the northeast Pacific, correlating with nutrient/chlorophyll fluctuations.
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{Tibble:
#'     \itemize{
#'       \item{\code{year}:} Year
#'       \item{\code{month}:} Month
#'       \item{\code{value}:} NPGO anomaly (index, unitless)
#'     }
#'   }
#'   \item{meta}{Includes:
#'     \itemize{
#'       \item{\code{data_type}:} "North Pacific Gyre Oscillation Index"
#'       \item{\code{region}:} "North Pacific"
#'       \item{\code{location_descriptor}:} "NPGO region"
#'       \item{\code{units}:} ""
#'       \item{\code{source_citation}:} "UCSD/SIO; see data-raw"
#'       \item{\code{original_value_col}:} "anomaly"
#'     }
#'   }
#' }
#' @docType data
#' @name npgo
#' @author Andrew Edwards
#' @source \url{http://www.o3d.org/npgo/}
"npgo"

#' Multivariate ENSO Index (MEI) (ea_data)
#'
#' The MEI is a bimonthly index formed by combining multiple atmospheric and oceanic variables, quantifying ENSO state in the tropical Pacific.
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{Tibble with:
#'     \itemize{
#'       \item{\code{year}:} Year
#'       \item{\code{month}:} Month (1–12, for bimonthly periods)
#'       \item{\code{value}:} MEI anomaly (unitless)
#'     }
#'   }
#'   \item{meta}{Includes:
#'     \itemize{
#'       \item{\code{data_type}:} "Multivariate ENSO Index"
#'       \item{\code{region}:} "Tropical Pacific"
#'       \item{\code{location_descriptor}:} "ENSO Basin"
#'       \item{\code{units}:} "index"
#'       \item{\code{source_citation}:} "NOAA PSL"
#'       \item{\code{original_value_col}:} "anomaly"
#'     }
#'   }
#' }
#' @docType data
#' @name mei
#' @author Andrew Edwards
#' @source \url{https://psl.noaa.gov/enso/mei/}
"mei"

#' Arctic Oscillation (AO) Index (ea_data)
#'
#' The AO Index tracks the primary mode of atmospheric circulation variability in the Northern Hemisphere, calculated via PCA of 1000 hPa geopotential anomalies north of 20°N.
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{Tibble:
#'     \itemize{
#'       \item{\code{year}:} Year
#'       \item{\code{month}:} Month (1–12)
#'       \item{\code{value}:} AO anomaly (unitless index)
#'     }
#'   }
#'   \item{meta}{Includes:
#'     \itemize{
#'       \item{\code{data_type}:} "Arctic Oscillation Index"
#'       \item{\code{region}:} "Northern Hemisphere"
#'       \item{\code{location_descriptor}:} "AO PCA"
#'       \item{\code{units}:} ""
#'       \item{\code{source_citation}:} "NOAA CPC"
#'       \item{\code{original_value_col}:} "anomaly"
#'     }
#'   }
#' }
#' 
#' @docType data
#' @name ao
#' @author Andrew Edwards
#' @source \url{https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/}
"ao"
 
#' Atlantic MultiDecadal Oscillation (AMO) Index (ea_data)
#'
#' 
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{Tibble:
#'     \itemize{
#'       \item{\code{year}:} Year
#'       \item{\code{month}:} Month (1–12)
#'       \item{\code{value}:} SSTA (sea surface temperature anomaly)
#'     }
#'   }
#'   \item{meta}{Includes:
#'     \itemize{
#'       \item{\code{data_type}:} "Atlantic Multidecadal Oscillation Index"
#'       \item{\code{region}:} "Northern Hemisphere (0-60N)"
#'       \item{\code{location_descriptor}:} "North Atlantic"
#'       \item{\code{units}:} ""
#'       \item{\code{source_citation}:} "NOAA , https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.amo.dat"
#'       \item{\code{original_value_col}:} "SSTA"
#'     }
#'   }
#' }
#' 
#' @docType data
#' @name amo
#' @author Emily O'Grady
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.amo.dat}
"amo"
 
#' Ecological Indicators (ea_data)
#'
#' Ecological indicators calculated from biomass estimates and commercial harvesting data.
#'
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{A tibble with columns:
#'     \describe{
#'       \item{year}{Year}
#'       \item{region}{NAFO division or SS area}
#'       \item{SpeciesRichness_ALL_value}{Species Richness from samples collected in RV survey}
#'       \item{ShannonDiveristy_ALL_value}{Index considering species richness and evenness of abundances}
#'       \item{MargalefRichness_ALL_value}{An index measure of richness accounting for sample size}
#'       \item{PielouEvenness_ALL_value}{How evenly species are distributed within a community}
#'       \item{HillDiversity_ALL_value}{Effective number of species}
#'       \item{HillDominance_ALL_value}{Community dominance index describing how much diversity is driven by a few dominant species}
#'       \item{Heips_ALL_value}{Relative abundance of different species in a community}
#'       \item{ABUNDANCE_ALL_value}{Total abundance}
#'       \item{BIOMASS_ALL_value}{Total biomass of all species caught in RV survey (t)}
#'       \item{BIOMASS_CLUPEIDS_value}{Total biomass of clupeid species (t)}
#'       \item{BIOMASS_FINFISH_value}{Total biomass of finfish species (t)}
#'       \item{BIOMASS_FLATFISH_value}{Total biomass of flatfish species (t)}
#'       \item{BIOMASS_FORAGE_value}{Total biomass of forage species (t)}
#'       \item{BIOMASS_GADOIDS_value}{Total biomass of gadoid species (t)}
#'       \item{BIOMASS_GROUNDFISH_value}{Total biomass of groundfish species (t)}
#'       \item{BIOMASS_PELAGIC_value}{Total biomass of pelagic species (t)}
#'       \item{BIOMASS_SKATES_value}{Total biomass of skate species (t)}
#'       \item{FishinginBalance_value}{Changes in fishing strategies and their impact on system productivity; positive FiB indicates fishery has expanded or bottom-up effects are occurring}
#'       \item{DiversityTargetSpp_ALL_value}{Diversity of target commercial species}
#'       \item{MeanTL.Landings_value}{Mean Trophic level of all landings}
#'       \item{MTI.Landings_3.25_value}{Mean Trophic level of landings above TL 3.25}
#'       \item{landings_ALL_value}{Total landings (t)}
#'       \item{landings_CLUPEIDS.L_value}{Total landings of clupeid species (t)}
#'       \item{landings_FINFISH.L_value}{Total landings of finfish species (t)}
#'       \item{landings_FLATFISH.L_value}{Total landings of flatfish species(t)}
#'       \item{landings_FORAGE.L_value}{Total landings of forage species (t)}
#'       \item{landings_GADOIDS.L_value}{Total landings of gadoid species (t)}
#'       \item{landings_GROUNDFISH.L_value}{Total landings of groundfish species (t)}
#'       \item{landings_INVERTEBRATES.L_value}{Total landings of invertebrate species (t)}
#'       \item{landings_LARGE_PELAGIC.L_value}{Total landings of pelagic species (t)}
#'       \item{landings_SKATES.L_value}{Total landings of skate species (t)}
#'       \item{FP_ALL_value}{Fishing pressure of all species}
#'       \item{FP_CLUPEIDS_value}{Fishing pressure of clupeid species}
#'       \item{FP_FINFISH_value}{Fishing pressure of finfish species}
#'       \item{FP_FLATFISH_value}{Fishing pressure of flatfish species}
#'       \item{FP_FORAGE_value}{Fishing pressure of forage species}
#'       \item{FP_GADOIDS_value}{Fishing pressure of gadoid species}
#'       \item{FP_GROUNDFISH_value}{Fishing pressure of groundfish species}
#'       \item{FP_SKATES_value}{Fishing pressure of skate species}
#'       \item{CVBiomass_value}{Coefficient of variation of the biomass}
#'       \item{MeanLifespan_value}{Mean lifespan observed for each species}
#'       \item{BIOMASS_TL2_value}{Biomass of trophic level 2}
#'       \item{BIOMASS_TL3_value}{Biomass of trophic level 3}
#'       \item{BIOMASS_TL4_value}{Biomass of trophic level 4}
#'       \item{MMLength_BIOMASS_value}{Mean maximum length in the community weighted by biomass}
#'       \item{MMLength_ABUNDANCE_value}{Mean maximum length in the community weighted by abundance}
#'       \item{IVILandings_value}{Intrinsic vulnerability index of the catch}
#'       \item{BIOMASS_LBENTHIVORE_value}{Biomass of large benthivores (t) calculated from RV survey}
#'       \item{BIOMASS_MBENTHIVORE_value}{Biomass of medium-size benthivores (t) calculated from RV survey}
#'       \item{BIOMASS_PISCIVORE_value}{Biomass of piscivores (t) calculated from RV survey}
#'       \item{BIOMASS_PLANKTIVORE_value}{Biomass of planktivores (t) calculated from RV survey}
#'       \item{BIOMASS_ZOOPISCIVORE_value}{Biomass of zoopiscivores (t) calculated from RV survey}
#'       \item{PELAGIC_GROUNDFISH_value}{Biomass of pelagics to biomass of groundfish}
#'       \item{PREDATORS_ALL_value}{Total biomass of predators}
#'       \item{LargeFishIndicator_value}{Proportion of large fish (> 35cm) to small fish (<= 35cm)}
#'       \item{MeanLengthBIOMASS_value}{Mean length of fish in the community weighted by biomass}
#'       \item{MeanLengthABUNDANCE_value}{Mean length of fish in the community weighted by abundance}
#'       \item{CCondition_FINFISH_value}{Community condition of finfish}
#'       \item{CCondition_LBENTHIVORE_value}{Community condition of large benthivores}
#'       \item{CCondition_MBENTHIVORE_value}{Community condition of medium benthivores}
#'       \item{CCondition_PISCIVORE_value}{Community condition of piscivores}
#'       \item{CCondition_PLANKTIVORE_value}{Community condition of planktivores}
#'       \item{CCondition_ZOOPISCIVORE_value}{Community condition of zoopiscivores}
#'       \item{BIOMASS_INVERTEBRATES_value}{Biomass of invertebrates}
#'       \item{INVERTEBRATES_GROUNDFISH_value}{Proportion of invertebrates to groundfish}
#'       \item{FP_INVERTEBRATES_value}{Fishing pressure of invertebrates}
#'     }
#'   }
#'   \item{meta}{A list with elements:
#'     \describe{
#'       \item{data_type}{"ecological"}
#'       \item{region}{"Maritimes"}
#'       \item{location_descriptor}{"NAFO"}
#'       \item{units}{"tonnes"}
#'       \item{species}{"NA"}
#'       \item{source_citation}{"Bundy et al. 2017"}
#'     }
#'   }
#' }
#'
#' @docType data
#' @name eco_indicators
#' @author Jamie C. Tam
#' @source Generated from running \code{data-raw/eco-indicators/eco-indicators.R}
"eco_indicators"