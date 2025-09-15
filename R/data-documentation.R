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
#'       \item{data_type}{Character. "Grey Seal Abundance"}
#'       \item{region}{Character. "Scotian Shelf"}
#'       \item{location_descriptor}{Character. "Sable Island / Scotian Shelf"}
#'       \item{units}{Character. "count"}
#'       \item{species}{Character. "Halichoerus grypus"}
#'       \item{source_citation}{Character. See details, Hammill et al. (2023)}
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

#' Food Habits Metadata
#'
#' Stomach content and food habits data collected during DFO ecosystem surveys in the Maritimes region. Contains trawl set, fish, and prey item information.
#'
#' @format A data.frame with columns:
#' \describe{
#'   \item{DATASOURCE}{Data source}
#'   \item{MISSION}{Cruise/mission identifier}
#'   \item{SETNO}{Set number}
#'   \item{SDATE}{Date of set}
#'   \item{STIME}{Time of set (24 hr)}
#'   \item{STRAT}{Stratum code}
#'   \item{BOTTOM_TEMPERATURE}{Water temperature at bottom (°C)}
#'   \item{DEPTH}{Bottom depth (m)}
#'   \item{GEAR}{Gear code}
#'   \item{SLATDD}{Latitude (decimal degrees)}
#'   \item{SLONGDD}{Longitude (decimal degrees, negative=west)}
#'   \item{NAFO_ZONE}{NAFO zone}
#'   \item{NAFO_SUBUNIT}{NAFO subunit}
#'   \item{SPEC}{Species code}
#'   \item{FSHNO}{Fish ID}
#'   \item{FWT}{Fish weight (g)}
#'   \item{FLEN}{Fish length (cm)}
#'   \item{STOWGT}{Stomach weight (g)}
#'   \item{EMPTYWGT}{Empty stomach weight (g)}
#'   \item{FULLNESS}{Stomach fullness (coded)}
#'   \item{FGEN}{Fish sex}
#'   \item{PREYSPECCD}{Prey species code}
#'   \item{PWT}{Prey weight (g)}
#'   \item{PLEN}{Prey length (cm)}
#'   \item{PNUM}{Number of prey}
#'   \item{DIGESTION}{Digestive state (coded)}
#' }
#' @docType data
#' @name food_habits
#' @author Manon Cassista Da Ros; Mike McMahon
#' @source Generated from running `data-raw/ecosystem-survey/food_habits.R`
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
 
#' ecological indicators (ea_data)
#'
#'ecological indicators calculated from biomass estimates and commercial harvesting data
#' @format An `ea_data` object:
#' \describe{
#'   \item{data}{Tibble with:
#'     \itemize{
#'       \item{\code{year}:} Year
#'       \item{\code{region}:} NAFO division or SS area
#'       \item{\code{value}:} Species Richness from samples collected in RV survey
#'       \item{\code{ShannonDiveristy_ALL}:} Index considering species richness and evenness of abundances
#'       \item{\code{MargalefRichness_ALL}:} An Index measure of richness accounting for sample size
#'       \item{\code{PielouEvenness_ALL}:} How evenly species are distributed within a community
#'       \item{\code{HillDiversity_ALL}:} effective number of species
#'       \item{\code{HillDominance_ALL}:} community dominance index describing how much a community's diversity is driven bye few dominant species
#'       \item{\code{Heips_ALL}:} relative abundance of different species in a community
#'       \item{\code{ABUNDANCE_ALL}:} Total abundance
#'       \item{\code{BIOMASS_ALL}:} Total biomass of all species caught in RV survey (t)
#'       \item{\code{BIOMASS_CLUPEIDS}:} Total biomass of clupeid species (t)
#'       \item{\code{BIOMASS_FINFISH}:} Total biomass of finfish species (t) 
#'       \item{\code{BIOMASS_FLATFISH}:} Total biomass of flatfish species (t)
#'       \item{\code{BIOMASS_FORAGE}:} Total biomass of forage species (t)
#'       \item{\code{BIOMASS_GADOIDS}:} Total biomass of gadoids species (t)
#'       \item{\code{BIOMASS_GROUNDFISH}:} Total biomass of groundfish species (t)
#'       \item{\code{BIOMASS_PELAGIC}:} Total biomass of pelagic species (t)
#'       \item{\code{BIOMASS_SKATES}:} Total biomass of skate species (t)
#'       \item{\code{FishinginBalance}:} changes in fishing strategies and their impact on system productivity; positive FiB indicates fishery has expanded or bottom-up effects are occurring
#'       \item{\code{FishinginBalance_s}:} standardized changes in fishing strategies and their impact on system productivity
#'       \item{\code{DiversityTargetSpp_ALL}:}} Diversity of target commercial species
#'   \item{\code{value}:}{Mean Trophic level of all landings}
#'   \item{\code{value}:}{Mean Trophic Level of landings  above TL 3.25}
#'   \item{\code{value}:}{total landings (t)}
#'   \item{\code{value}:}{total landings of clupeids species (t)}
#'   \item{\code{value}:}{total landings of finfish species (t)}
#'   \item{\code{value}:}{total landings of flatfish species(t)}
#'   \item{{\code{value}:}{total landings of forage species (t)}
#'   \item{\code{value}:}{total landings of gadoid species (t))}
#'   \item{{\code{value}:}{total landings of groundfish species (t)}
#'   \item{\code{value}:}{total landings of invertebrate species (t)}
#'   \item{\code{value}:}{total landings of pelagic species (t)}
#'   \item{\code{value}:}{total landings of skate species (t)}
#'   \item{\code{value}:}{Fishing pressure of all species}
#'   \item{\code{value}:}{Fishing pressure of clupeid species}
#'   \item{{\code{value}:}{Fishing pressure of finfish species}
#'   \item{\code{value}:}{Fishing pressure of flatfish species}
#'   \item{\code{value}:}{Fishing pressure of forage species}
#'   \item{\code{value}:}{Fishing pressure of gadoid species}
#'   \item{\code{value}:}{Fishing pressure of groundfish species}
#'   \item{{\code{value}:}{Fishing pressure of skate species}
#'   \item{\code{value}:}{standardized diversity of target species}
#'   \item{\code{value}:}{standardize mean trophic level of landings}
#'   \item{\code{value}:}{standardized mean trophic level above 3.25}
#'   \item{\code{value}:}{standardized landings of all species}
#'   \item{landings_CLUPEIDS.L_s}{standardized landings of clupeid species}
#'   \item{landings_FINFISH.L_s}{standardized landings of finfish species (t)}
#'   \item{landings_FLATFISH.L_s}{standardized landings of flatfish species(t)}
#'   \item{landings_FORAGE.L_s}{standardized landings of forage species}
#'   \item{landings_GADOIDS.L_s}{standardized landings of gadoid species}
#'   \item{landings_GROUNDFISH.L_s}{standardized landings of groundfish species}
#'   \item{landings_INVERTEBRATES.L_s}{standardized landings of invertebrate species}
#'   \item{landings_LARGE_PELAGIC.L_s}{standardized landings of large pelagic species}
#'   \item{landings_SKATES.L_s}{standardized landings of skates species}
#'   \item{FP_ALL_s}{standardized fishing pressure of all species}
#'   \item{FP_CLUPEIDS_s}{standardized fishing pressure of clupeid species}
#'   \item{FP_FINFISH_s}{standardized fishing pressure of finfish species}
#'   \item{FP_FLATFISH_s}{standardized fishing pressure of flatfish species}
#'   \item{FP_FORAGE_s}{standardized fishing pressure of forage species}
#'   \item{FP_GADOIDS_s}{standardized fishing pressure of gadoid species}
#'   \item{FP_GROUNDFISH_s}{standardized fishing pressure of groundfish species}
#'   \item{FP_SKATES_s}{standardized fishing pressure of skates species}
#'   \item{CVBiomass}{Coefficient of Variation of the biomass}
#'   \item{MeanLifespan}{Mean Lifespan observed for each species}
#'   \item{BIOMASS_TL3}{biomass of trophic levle 3}
#'   \item{BIOMASS_TL4}{biomass of trophic level 4}
#'   \item{MMLength_BIOMASS}{Mean maxium length in the community weigthed by biomass}
#'   \item{MMLength_ABUNDANCE}{Mean maximum length in the community weighted by abundance}
#'   \item{IVILandings}{Intrinsic vulnerability index of the catch}
#'   \item{CVBiomass_s}{standardized coefficient of variation of biomass}
#'   \item{MeanLifespan_s}{standardized mean lifespan}
#'   \item{BIOMASS_TL2_s}{standardized biomass of trophic level 2}
#'   \item{BIOMASS_TL3_s}{standardized biomass of trophic level 3}
#'   \item{BIOMASS_TL4_s}{standardized biomass of trophic level 4}
#'   \item{MMLength_BIOMASS_s}{standardized mean maxium length in the community weigthed by biomass}
#'   \item{MMLength_ABUNDANCE_s}{standardized mean maxium length in the community weigthed by abundance}
#'   \item{IVILandings_s}{standardized Intrinsic vulnerability index of the catch}
#'   \item{BIOMASS_LBENTHIVORE}{biomass of large benthovores (t) calculated from RV survey}
#'   \item{BIOMASS_MBENTHIVORE}{biomass of medium size benthivores (t) calculated from RV survey}
#'   \item{BIOMASS_PISCIVORE}{biomass of piscivores (t) calculated from RV survey}
#'   \item{BIOMASS_PLANKTIVORE}{biomass of planktivores (t) calculated from RV survey}
#'   \item{BIOMASS_ZOOPISCIVORE}{biomass of zoopiscivores (t) calculated from RV survey}
#'   \item{PELAGIC_GROUNDFISH}{biomass of pelagics to biomass of groundfish}
#'   \item{PREDATORS_ALL}{Total biomass of predators}
#'   \item{LargeFishIndicator}{proportion of large fish > 35cm to small fish below or equal to 35cm}
#'   \item{MeanLengthBIOMASS}{mean length of fish in the community weighted by biomass}
#'   \item{MeanLengthABUNDANCE}{mean length of fish in the community weighted by abundance}
#'   \item{CCondition_FINFISH}{community condition of finfish}
#'   \item{CCondition_LBENTHIVORE}{community condition of large benthivores}
#'   \item{CCondition_MBENTHIVORE}{community condition of medium benthivores}
#'   \item{CCondition_PISCIVORE}{community condition of piscivores}
#'   \item{CCondition_PLANKTIVORE}{community condition of planktivores}
#'   \item{CCondition_ZOOPISCIVORE}{community condition of zoopiscivores}
#'   \item{BIOMASS_LBENTHIVORE_s}{standardized biomass of large benthivores}
#'   \item{BIOMASS_MBENTHIVORE_s}{standardized biomass of medium benthivores}
#'   \item{BIOMASS_PISCIVORE_s}{standardized biomass of piscivores}
#'   \item{BIOMASS_PLANKTIVORE_s}{standardized biomass of planktivores}
#'   \item{BIOMASS_ZOOPISCIVORE_s}{standardized biomass of zoopiscivores}
#'   \item{PELAGIC_GROUNDFISH_s}{standardized pelagic to groundfish}
#'   \item{PREDATORS_ALL_s}{standardized predoatrs}
#'   \item{LargeFishIndicaor_s}{standardized large fish indicator}
#'   \item{MeanLengthBIOMASS_s}{standardized mean length biomass}
#'   \item{MeanLengthABUNDANCE_s}{standardized mean length abundance}
#'   \item{CCondition_FINFISH_s}{standardized community condition of finfish}
#'   \item{CCondition_LBENTHIVORE_s}{standardized community condition of large benthivores}
#'   \item{CCondition_MBENTHIVORE_s}{standardized community condition of medium benthivores}
#'   \item{CCondition_PISCIVORE_s}{standardized community condition of piscivores}
#'   \item{CCondition_PLANKTIVORE_s}{standardized community condition of planktivores}
#'   \item{CCondition_PISCIVORE_s}{standardized community condition of piscivores}
#'   \item{CCondition_PLANKTIVORE_s}{standardized community condition of planktivores}
#'   \item{CCondition_ZOOPISCIVORE_s}{standardized community condition of zooplanktivores}
#'   \item{BIOMASS_INVERTEBRATES}{biomass of invertebrates}
#'   \item{INVERTEBRATES_GROUNDFISH}{proportion of invertebrates to groundfish}
#'   \item{FP_INVERTEBRATES}{Fishing pressure of invertebrates}
#'   \item{BIOMASS_TL2}{biomass of trophic level 2}
#'       
#'     }
#'   }
#'   \item{meta}{List includes:
#'     \itemize{
#'       \item{\code{data_type}:} "diversity"
#'       \item{\code{location_descriptor}:} "Maritimes Region"
#'       \item{\code{units}:} tonnes
#'     }
#'   }
#' }
#'
#' 
#' @docType data
#' @name eco_indicators
#' @author Jamie C. Tam
#' @source Generated from running `data-raw/eco-indicators/eco-indicators.R`
"eco_indicators"