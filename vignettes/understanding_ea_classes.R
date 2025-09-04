## ----setup, echo = FALSE, warnings = FALSE, message = FALSE-------------------
library(marea)
library(tidyverse)

## -----------------------------------------------------------------------------
# Load grey seal abundance data
data("grey_seals")

# Print object summary (metadata, structure, preview)
ea.print(grey_seals)

## -----------------------------------------------------------------------------
ea.summary(grey_seals)

## -----------------------------------------------------------------------------
# plot(grey_seals)

## -----------------------------------------------------------------------------
# plot(grey_seals, style = "ribbon")

