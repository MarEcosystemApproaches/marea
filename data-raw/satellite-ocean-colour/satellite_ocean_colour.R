rm(list=ls())
library(rerddap)
library(sf)
library(dplyr)
library(lubridate)
library(marea)

# bloom metrics available in the satellite product
metrics <- list(t_start = list(data_type="Day of year of the start of the spring phytoplankton bloom",
                               units="day of year"),
                t_duration = list(data_type="Duration of the spring phytoplankton bloom",
                                  units="days"),
                amplitude_real = list(data_type="Maximum concentration during the spring phytoplankton bloom period",
                                      units="mg/m3"),
                magnitude_real = list(data_type="Total chlorophyll-a produced during the spring phytoplankton bloom period",
                                      units="days*mg/m3"),
                annual_mean = list(data_type="Average chlorophyll-a over the year",
                                   units="mg/m3"),
                NRMSE_bloom = list(data_type="Root mean square error between the fitted Gaussian and real chl-a values during the bloom period, normalized to amplitude_real",
                                   units="mg/m3"),
                percent_dineof = list(data_type="Percentage of days with pixel values estimated using DINEOF",
                                      units=""))


#*******************************************************************************
# DOWNLOAD AND FILTER ####

# download the full dataset
df <- get_erddap_data(variables=names(metrics))

# filter out NRMSE_bloom > 0.5
bad_inds <- df$NRMSE_bloom > 0.5
bad_inds[!is.finite(bad_inds)] <- TRUE
df[bad_inds,c("t_start","t_duration","amplitude_real","magnitude_real","NRMSE_bloom")] <- NA

# filter out |standardized_anomaly| > 4
stdevnum <- 4
dfsa <- df %>%
  dplyr::group_by(longitude,latitude) %>%
  dplyr::mutate(amplitude_real_climmean=mean(amplitude_real,na.rm=TRUE),
                amplitude_real_climsd=sd(amplitude_real,na.rm=TRUE),
                annual_mean_climmean=mean(annual_mean,na.rm=TRUE),
                annual_mean_climsd=sd(annual_mean,na.rm=TRUE),
                magnitude_real_climmean=mean(magnitude_real,na.rm=TRUE),
                magnitude_real_climsd=sd(magnitude_real,na.rm=TRUE),
                t_duration_climmean=mean(t_duration,na.rm=TRUE),
                t_duration_climsd=sd(t_duration,na.rm=TRUE),
                t_start_climmean=mean(t_start,na.rm=TRUE),
                t_start_climsd=sd(t_start,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(amplitude_real_sanom=(amplitude_real-amplitude_real_climmean)/amplitude_real_climsd,
                annual_mean_sanom=(annual_mean-annual_mean_climmean)/annual_mean_climsd,
                magnitude_real_sanom=(magnitude_real-magnitude_real_climmean)/magnitude_real_climsd,
                t_duration_sanom=(t_duration-t_duration_climmean)/t_duration_climsd,
                t_start_sanom=(t_start-t_start_climmean)/t_start_climsd) %>%
  dplyr::mutate(anySAGTstdevnum = ifelse(is.finite(t_start_sanom),
                                         abs(amplitude_real_sanom)>stdevnum | abs(annual_mean_sanom)>stdevnum | abs(magnitude_real_sanom)>stdevnum | abs(t_duration_sanom)>stdevnum | abs(t_start_sanom)>stdevnum,
                                         abs(annual_mean_sanom)>stdevnum))
bad_inds <- dfsa$anySAGTstdevnum
bad_inds[!is.finite(bad_inds)] <- TRUE
df[bad_inds,names(metrics)] <- NA

# make sure lat/lon columns are in the correct format
df <- df %>% dplyr::mutate(longitude=as.numeric(longitude),
                           latitude=as.numeric(latitude))


#**************************
# get the dataset into from the server
dataset_info <- rerddap::info("bio_remote_sensing_occci_nwa_poly4_spring_bloom", url="https://cioosatlantic.ca/erddap/")

#**************************
# get dataset boundaries from erddap
xlim <- as.numeric(strsplit(dataset_info$alldata$longitude$value[3],split=", ")[[1]])
ylim <- as.numeric(strsplit(dataset_info$alldata$latitude$value[3],split=", ")[[1]])
zlim <- format(as_datetime(as.numeric(strsplit(dataset_info$alldata$time$value[3],split=", ")[[1]])),"%Y-%m-%dT00:00:00Z")
field <- "all"

#**************************
# download the data from the server
df <- griddap(dataset_info, latitude=ylim, longitude=xlim, time=zlim, fields=field)$data %>%
  dplyr::mutate(time=as_datetime(time))

#**************************
# FILTER OUT NRMSE_BLOOM > 0.5
bad_inds <- df$NRMSE_bloom > 0.5
bad_inds[!is.finite(bad_inds)] <- TRUE
df[bad_inds,c("t_start","t_duration","amplitude_real","magnitude_real","NRMSE_bloom")] <- NA

#**************************
# FILTER OUT |STANDARDIZED_ANOMALY| > 4
stdevnum <- 4
dfsa <- df %>%
  dplyr::group_by(longitude,latitude) %>%
  dplyr::mutate(amplitude_real_climmean=mean(amplitude_real,na.rm=TRUE),
                amplitude_real_climsd=sd(amplitude_real,na.rm=TRUE),
                annual_mean_climmean=mean(annual_mean,na.rm=TRUE),
                annual_mean_climsd=sd(annual_mean,na.rm=TRUE),
                magnitude_real_climmean=mean(magnitude_real,na.rm=TRUE),
                magnitude_real_climsd=sd(magnitude_real,na.rm=TRUE),
                t_duration_climmean=mean(t_duration,na.rm=TRUE),
                t_duration_climsd=sd(t_duration,na.rm=TRUE),
                t_start_climmean=mean(t_start,na.rm=TRUE),
                t_start_climsd=sd(t_start,na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(amplitude_real_sanom=(amplitude_real-amplitude_real_climmean)/amplitude_real_climsd,
                annual_mean_sanom=(annual_mean-annual_mean_climmean)/annual_mean_climsd,
                magnitude_real_sanom=(magnitude_real-magnitude_real_climmean)/magnitude_real_climsd,
                t_duration_sanom=(t_duration-t_duration_climmean)/t_duration_climsd,
                t_start_sanom=(t_start-t_start_climmean)/t_start_climsd) %>%
  dplyr::mutate(anySAGTstdevnum = ifelse(is.finite(t_start_sanom),
                                         abs(amplitude_real_sanom)>stdevnum | abs(annual_mean_sanom)>stdevnum | abs(magnitude_real_sanom)>stdevnum | abs(t_duration_sanom)>stdevnum | abs(t_start_sanom)>stdevnum,
                                         abs(annual_mean_sanom)>stdevnum))
bad_inds <- dfsa$anySAGTstdevnum
bad_inds[!is.finite(bad_inds)] <- TRUE
df[bad_inds,metrics] <- NA

#**************************
# Convert to ea_spatial

for (metric in metrics) {
  
  times <- sort(unique(df$time))
  
  # convert the data for this metric into a multilayer spatraster (each layer in a single time unit)
  r <- lapply(times, function(ti) {
    df %>%
      dplyr::filter(time==ti) %>%
      dplyr::select(longitude,latitude,all_of(metric)) %>%
      dplyr::mutate(longitude=as.numeric(longitude), latitude=as.numeric(latitude)) %>%
      tidyterra::as_spatraster(xycols=1:2,crs="EPSG:4326")
  }) %>% do.call(what=c) %>% setNames(paste0(names(.),"_",year(as_date(times))))
  time(r) <- times
  
  r_ea_spatial <- as_ea_spatial(
    x = r,
    value_col = names(r),
    data_type    = "Phytoplankton spring bloom initiation day",
    region       = "Northwest Atlantic",
    time_descriptor = "Annual",
    units        = "Day of year",
    source_citation = "SOPhyE group, DFO",
    temporal_coverage = paste0(year(range(time(r))),collapse="-"),
    variable_name= metric
  )
  # Assign the object to a variable with the name from metric
  # TODO @ steph  maybe preface the name with something consistent so data object can be 
  # grouped together? ie. paste0('sophye_', metric) @
  assign(metric, r_ea_spatial)
  
  # Save the data with the metric name
  do.call(usethis::use_data, list(as.name(metric), overwrite = TRUE, internal = FALSE))
  
  }

