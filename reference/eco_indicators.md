# ecological indicators (ea_data)

ecological indicators calculated from biomass estimates and commercial
harvesting data

## Usage

``` r
data(azmp_salinity)
```

## Format

an `ea_data` object with multiple columns:

- year:

  Year of data collection

- region:

  assessment of management area, NAFO, Scotian Shelf

- SpeciesRichness_ALL:

  The number of species recorded in RV survey or commercial landings

- ShannonDiveristy_ALL:

  Index considering species richness and evenness of abundances

- MargalefRichness_ALL:

  An Index measure of richness accounting for sample size

- PielouEvenness_ALL:

  How evenly species are distributed within a community

- HillDiversity_ALL:

  effective number of species

- HillDominance_ALL:

  community dominance index describing how much a community's diveristy
  is driven bye few dominant species

- Heips_ALL:

  relative abundance of different species in a community

- SpeciesRichness_ALL_s:

  standardized species richness

- ShannonDiversity_ALL_s:

  sandardized Shannon diveristy index

- MargalefRichness_ALL_s:

  sandardized Margalef species richness

- PielouEvenness_ALL_s:

  standardized Pilou's evenness

- HillDiversity_ALL_s:

  standardized Hill diversity

- HillDominance_ALL_S:

  standardized Hill dominance

- Heips_ALL_s:

  standardized Heips

- Abundance_ALL:

  Total abundance

- BIOMASS_ALL:

  Total biomass of all species caught in RV survey (t)

- BIOMASSS_CLUPEIDS:

  Total biomass of clupeid species (t)

- BIIOMASS_FINFISH:

  Total biomass of finfish species (t)

- BIOMNASS_FLATFISH:

  Total biomass of flatfish species (t)

- BIOMASS_FORAGE:

  Total biomass of forage species (t)

- BIOMASS_GADOIDS:

  Total biomass of gadoids species (t)

- BIOMASS_GROUNDFISH:

  Total biomass of groundfish species (t)

- BIOMASS_PELAGIC:

  Total biomass of pelagic species (t)

- BIOMASS_SKATE:

  Total biomass of skate species (t)

- FishinginBalance:

  changes in fishing strategies and their impact on system productivity;
  positive FiB indicates fishery has expanded or bottom-up effects are
  occuring

- ABUNDANCE_ALL_s:

  standardized total abundance of all species caught in RV survey

- BIOMASS_ALL_s:

  standardized total biomass of all species caught in RV survey

- BIOMASS_CLUPEIDS_s:

  standardized total biomass of all clupeid species

- BIOMASS_FINFISH_s:

  standardized total biomass of all finsfish species

- BIOMASS_FLATFISH_s:

  standardized total biomass of all flatfish species

- BIOMASS_FORAGE_s:

  standardized total biomass of all forage species

- BIOMASS_GADOIDS_s:

  standardized total biomass of all gadioid species

- BIOMASS_GROUNDFISH_s:

  standardized total biomass of all groundfish species

- BIOMASS_PELAGIC_s:

  standardized total biomass of all pelagic species

- BIOMASS_SKATES_s:

  standardized total biomass of all akate species

- FishinginBaland_s:

  standardized changes in fishing strategies and their impact on system
  productivity

- DiversityTargetSpp_ALL:

  Diversity of target commercial species

- MeanTL.Landings:

  Mean Trophic level of all landings

- MTI.Landings_3.25:

  Mean Trophic Level of landings above TL 3.25

- landings_ALL:

  total landings (t)

- landings_CLUPEIDS.L:

  total landings of clupeids species (t)

- landings_FINFISH.L:

  total landings of finfish species (t)

- landings_FLATFISH.L:

  total landings of flatfish species(t)

- landings_FORAGE.L:

  total landings of forage species (t)

- landings_GADOIDS.L:

  total landings of gadoid species (t))

- landings_GROUNDFISH.L:

  total landings of groundfish species (t)

- landings_INVERTEBRATES.L:

  total landings of invertebrate species (t)

- landings_LARGE_PELAGIC.L:

  total landings of pelagic species (t)

- landings_SKATES.L:

  total landings of skate species (t)

- FP_ALL:

  Fishing pressure of all species

- FP_CLUPEIDS:

  Fishing pressure of clupeid species

- FP_FINFISH:

  Fishing pressure of finfish species

- FP_FLATFISH:

  Fishing pressure of flatfish species

- FP_FORAGE:

  Fishing pressure of forage species

- FP_GADOIDS:

  Fishing pressure of gadoid species

- FP_GROUNDFISH:

  Fishing pressure of groundfish species

- FP_SKATES:

  Fishing pressure of skate species

- DiversityTargetSpp_ALL_s:

  standardized diversity of target species

- MeanTL.Landings_s:

  standardize mean trophic level of landings

- MTI.Landings_3.25_s:

  standardized mean trophic level above 3.25

- landings_ALL_s:

  standardized landings of all species

- landings_CLUPEIDS.L_s:

  standardized landings of clupeid species

- landings_FINFISH.L_s:

  standardized landings of finfish species (t)

- landings_FLATFISH.L_s:

  standardized landings of flatfish species(t)

- landings_FORAGE.L_s:

  standardized landings of forage species

- landings_GADOIDS.L_s:

  standardized landings of gadoid species

- landings_GROUNDFISH.L_s:

  standardized landings of groundfish species

- landings_INVERTEBRATES.L_s:

  standardized landings of invertebrate species

- landings_LARGE_PELAGIC.L_s:

  standardized landings of large pelagic species

- landings_SKATES.L_s:

  standardized landings of skates species

- FP_ALL_s:

  standardized fishing pressure of all species

- FP_CLUPEIDS_s:

  standardized fishing pressure of clupeid species

- FP_FINFISH_s:

  standardized fishing pressure of finfish species

- FP_FLATFISH_s:

  standardized fishing pressure of flatfish species

- FP_FORAGE_s:

  standardized fishing pressure of forage species

- FP_GADOIDS_s:

  standardized fishing pressure of gadoid species

- FP_GROUNDFISH_s:

  standardized fishing pressure of groundfish species

- FP_SKATES_s:

  standardized fishing pressure of skates species

- CVBiomass:

  Coefficient of Variation of the biomass

- MeanLifespan:

  Mean Lifespan observed for each species

- BIOMASS_TL3:

  biomass of trophic levle 3

- BIOMASS_TL4:

  biomass of trophic level 4

- MMLength_BIOMASS:

  Mean maxium length in the community weigthed by biomass

- MMLength_ABUNDANCE:

  Mean maximum length in the community weighted by abundance

- IVILandings:

  Intrinsic vulnerability index of the catch

- CVBiomass_s:

  standardized coefficient of variation of biomass

- MeanLifespan_s:

  standardized mean lifespan

- BIOMASS_TL2_s:

  standardized biomass of trophic level 2

- BIOMASS_TL3_s:

  standardized biomass of trophic level 3

- BIOMASS_TL4_s:

  standardized biomass of trophic level 4

- MMLength_BIOMASS_s:

  standardized mean maxium length in the community weigthed by biomass

- MMLength_ABUNDANCE_s:

  standardized mean maxium length in the community weigthed by abundance

- IVILandings_s:

  standardized Intrinsic vulnerability index of the catch

- BIOMASS_LBENTHIVORE:

  biomass of large benthovores (t) calculated from RV survey

- BIOMASS_MBENTHIVORE:

  biomass of medium size benthivores (t) calculated from RV survey

- BIOMASS_PISCIVORE:

  biomass of piscivores (t) calculated from RV survey

- BIOMASS_PLANKTIVORE:

  biomass of planktivores (t) calculated from RV survey

- BIOMASS_ZOOPISCIVORE:

  biomass of zoopiscivores (t) calculated from RV survey

- PELAGIC_GROUNDFISH:

  biomass of pelagics to biomass of groundfish

- PREDATORS_ALL:

  Total biomass of predators

- LargeFishIndicator:

  proportion of large fish \> 35cm to small fish below or equal to 35cm

- MeanLengthBIOMASS:

  mean length of fish in the community weighted by biomass

- MeanLengthABUNDANCE:

  mean length of fish in the community weighted by abundance

- CCondition_FINFISH:

  community condition of finfish

- CCondition_LBENTHIVORE:

  community condition of large benthivores

- CCondition_MBENTHIVORE:

  community condition of medium benthivores

- CCondition_PISCIVORE:

  community condition of piscivores

- CCondition_PLANKTIVORE:

  community condition of planktivores

- CCondition_ZOOPISCIVORE:

  community condition of zoopiscivores

- BIOMASS_LBENTHIVORE_s:

  standardized biomass of large benthivores

- BIOMASS_MBENTHIVORE_s:

  standardized biomass of medium benthivores

- BIOMASS_PISCIVORE_s:

  standardized biomass of piscivores

- BIOMASS_PLANKTIVORE_s:

  standardized biomass of planktivores

- BIOMASS_ZOOPISCIVORE_s:

  standardized biomass of zoopiscivores

- PELAGIC_GROUNDFISH_s:

  standardized pelagic to groundfish

- PREDATORS_ALL_s:

  standardized predoatrs

- LargeFishIndicaor_s:

  standardized large fish indicator

- MeanLengthBIOMASS_s:

  standardized mean length biomass

- MeanLengthABUNDANCE_s:

  standardized mean length abundance

- CCondition_FINFISH_s:

  standardized community condition of finfish

- CCondition_LBENTHIVORE_s:

  standardized community condition of large benthivores

- CCondition_MBENTHIVORE_s:

  standardized community condition of medium benthivores

- CCondition_PISCIVORE_s:

  standardized community condition of piscivores

- CCondition_PLANKTIVORE_s:

  standardized community condition of planktivores

- CCondition_PISCIVORE_s:

  standardized community condition of piscivores

- CCondition_PLANKTIVORE_s:

  standardized community condition of planktivores

- CCondition_ZOOPISCIVORE_s:

  standardized community condition of zooplanktivores

- BIOMASS_INVERTEBRATES:

  biomass of invertebrates

- INVERTEBRATES_GROUNDFISH:

  proportion of invertebrates to groundfish

- FP_INVERTEBRATES:

  Fishing pressure of invertebrates

- BIOMASS_TL2:

  biomass of trophic level 2

## Source

Generated from running `data-raw/eco-indicators/eco-indicators.R`

## Author

Jamie C. Tam
