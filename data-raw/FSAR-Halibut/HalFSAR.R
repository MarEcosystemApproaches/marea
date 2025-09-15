###playing around with FSAR data to get a four-plot


library(tidyverse)
library(marea)
library(here)
library(patchwork)

HalFSAR <- readRDS("C:/Users/tamj/Downloads/HalFSAR.rds")

## code from Mike M. to pull the upper and lower bounds from long to wide format

bases_with_bounds <- c("HSpred", "HSproj", "Ut", "Mt", "RVpred")

# Strip possible suffix from ts.name
HalFSAR2 <- HalFSAR %>%
  mutate(
    base = str_replace(ts.name, "(low|high)$", "")
  )

# For each panel.category/year/base, find low/high values
HalFSAR_aug <- HalFSAR2 %>%
  group_by(panel.category, year, base) %>%
  mutate(
    low  = if (base[1] %in% bases_with_bounds) ts.value[ts.name == paste0(base[1], "low")][1] else NA_real_,
    high = if (base[1] %in% bases_with_bounds) ts.value[ts.name == paste0(base[1], "high")][1] else NA_real_
  ) %>%
  ungroup()

# Now keep only the main (non-low/non-high) rows
# The main names are: base values (not ending in low/high)
HalFSAR <- HalFSAR_aug %>%
  filter(ts.name == base) %>%
  select(panel.category, year, ts.name, ts.value, low, high)


fsar_halibut<-ea_data(
  data=HalFSAR,
  value_col="ts.value",
  data_type= "stock",
  location_descriptor = "NAFO3nops4vswx5zc",
  region = "Maritimes",
  units="variable")

#make plots
Fishingmortality<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Fishing" & fsar_halibut@data$ts.name=="Ut"
  ], style="ribbon") +
      labs(title="Fishing Mortality", y="Mortality (1/yr)", subtitle="")

halibutbiomass<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Biomass" & fsar_halibut@data$ts.name=="HSpred"
  ], style="ribbon") +
       labs(title="Halibut Survey Biomass", y="Biomass (kt)", subtitle="") +
        geom_hline (yintercept=10.5, color="red", linetype="dashed") +
        geom_hline (yintercept=22, color="green", linetype="dashed") 

halibutcatch<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Catch" & fsar_halibut@data$ts.name=="CanadaTotal"
  ]) +
       labs(title="Halibut 3NOPs4VWX5Zc", y="Catch (t)", subtitle="")

halibutabundance<-plot(fsar_halibut[
  fsar_halibut@data$panel.category=="Recruitment" & fsar_halibut@data$ts.name=="RVpred"
  ], style="ribbon") +
      labs(title="RV Survey (modeled)", y="Abundance (millions)", subtitle="")

#use patchwork to make 4plot
(halibutcatch + halibutbiomass)/(Fishingmortality + halibutabundance)

#save data as formal ea_data class
save(fsar_halibut, file= here::here("data-raw", "FSAR-Halibut", "fsar_halibut.Rdata"))