###FSAR fourplot example


library(tidyverse)
library(readr)
library(marea)
library(here)
library(patchwork)

# specify file location, load example .csv
fsar_fourplot_exdata <- read_csv("data-raw/fsar-example/fsar_fourplot_exdata.csv")

# create ea_data() from loaded data
fsar_example<-ea_data(
  data=fsar_fourplot_exdata,
  value_col="value",
  data_type= "stock",
  location_descriptor = "NAFO divisions",
  region = "Maritimes",
  units="variable")

#make plots
pF<-plot(fsar_example[
  fsar_example@data$panel.category=="Fishing" & fsar_example@data$ts.name=="Ut"
], style="ribbon") +
  labs(title="Fishing Mortality", y="Mortality (1/yr)", subtitle="")

pbiomass<-plot(fsar_example[
  fsar_example@data$panel.category=="Biomass" & fsar_example@data$ts.name=="Survey"
], style="ribbon") +
  labs(title="Biomass", y="Biomass (kt)", subtitle="") +
  geom_hline (yintercept=10.5, color="red", linetype="dashed") +
  geom_hline (yintercept=22, color="green", linetype="dashed") 

pcatch<-plot(fsar_example[
  fsar_example@data$panel.category=="Catch" & fsar_example@data$ts.name=="CanadaTotal"
]) +
  labs(title="Fish NAFO div", y="Catch (t)", subtitle="")

pabun<-plot(fsar_example[
  fsar_example@data$panel.category=="Recruitment" & fsar_example@data$ts.name=="RVpred"
], style="ribbon") +
  labs(title="RV Survey (modeled)", y="Abundance (millions)", subtitle="")

#use patchwork to make 4plot
(pcatch+ pbiomass)/(pF + pabun)

