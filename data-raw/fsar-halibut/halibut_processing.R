HalFSAR_raw<- readRDS("R:/<wherever Jamie put the file>/HalFSAR.rds")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

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

usethis::use_data(fsar_halibut)

plot(fsar_halibut[fsar_halibut@data$panel.category=="Fishing" & fsar_halibut@data$ts.name=="Mt"], style="ribbon")   +   labs(title="Fishing:Mt")
plot(fsar_halibut[fsar_halibut@data$panel.category=="Fishing" & fsar_halibut@data$ts.name=="Ut"], style="ribbon")   +   labs(title="Fishing:Ut")
plot(fsar_halibut[fsar_halibut@data$panel.category=="Biomass" & fsar_halibut@data$ts.name=="HSpred"], style="ribbon")   +   labs(title="Biomass:HSpred")
plot(fsar_halibut[fsar_halibut@data$panel.category=="Biomass" & fsar_halibut@data$ts.name=="HSproj"], style="ribbon")   +   labs(title="Biomass:HSProj")
plot(fsar_halibut[fsar_halibut@data$panel.category=="Recruitment" & fsar_halibut@data$ts.name=="RVpred"], style="ribbon")   +   labs(title="Recruitment:RVpred")

