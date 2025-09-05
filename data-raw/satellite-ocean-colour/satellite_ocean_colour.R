

library(rerddap)
library(terra)
library(sf)
library(dplyr)
library(lubridate)

# bloom metrics available in the satellite product
metrics <- c("t_start","t_duration","amplitude_real","magnitude_real","annual_mean","NRMSE_bloom","percent_dineof")


# download the data from the CIOOS Atlantic ERDDAP server


# need to fix this so you can download the whole dataset if you want, instead of subsetting


xlim <- c(-65,-60)
ylim <- c(42,46)
zlim <- c("1998-01-01T00:00:00Z", "2024-01-01T00:00:00Z") # zlim <- c("first","last") to download the full time series
field <- "all" # field <- "all" if you want all of the fields

# get the dataset into from the server
dataset_info <- rerddap::info("bio_remote_sensing_occci_nwa_poly4_spring_bloom", url="https://cioosatlantic.ca/erddap/")

# download the data from the server
df <- griddap(dataset_info, latitude=ylim, longitude=xlim, time=zlim, fields=field)$data %>%
  dplyr::mutate(time=as_datetime(time))

# for each metric, convert lon/lat/time dataframe to multilayer spatraster (each layer is a year)
r <- lapply(metrics, function(m) {
  df %>%
    dplyr::select(longitude,latitude,time,all_of(m)) %>%
    reshape(timevar="time", idvar=c("longitude","latitude"), direction="wide") %>%
    terra::rast(type="xyz") %>%
    # set layer names to year
    setNames(year(as_date(gsub(paste0(m,"."),"",names(.)))))
}) %>% setNames(metrics)

# calculate climatologies
rclim <- lapply(metrics, function(m) {
  rmean <- terra::mean(r[[m]],na.rm=TRUE)
  rsd <- terra::stdev(r[[m]],na.rm=TRUE)
  return(list(climatology_mean=rmean, climatology_sd=rsd))
}) %>% setNames(metrics)

# calculate standardized anomalies
ranom <- lapply(metrics, function(m) {
  rtmp <- r[[m]]
  rclimtmp <- rclim[[m]]
  ranom <- (rtmp-rclimtmp$climatology_mean)/rclimtmp$climatology_sd
  return(ranom)
}) %>% setNames(metrics)

