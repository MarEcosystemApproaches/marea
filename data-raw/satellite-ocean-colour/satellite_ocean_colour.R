rm(list=ls())
library(rerddap)
library(sf)
library(dplyr)
library(lubridate)
library(marea)
library(stars)

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


#*******************************************************************************
# WRITE TO OUTPUT ####

# metrics to include in the output
metrics_output <- names(metrics)[!(names(metrics) %in% c("NRMSE_bloom","percent_dineof"))]

# variables for ea_spatial attributes
temporal_cov <- paste0(year(range(df$time)),collapse="-")
data_types <- sapply(metrics_output, function(m) paste0(m,": ",metrics[[m]]$data_type)) %>%
  paste0(collapse="; ")
units <- sapply(metrics_output, function(m) paste0(m,": ",metrics[[m]]$units)) %>%
  paste0(collapse="; ")

# convert the data for this metric into a stars object and reduce resolution to 0.1 degrees
r <- df %>%
  dplyr::select(longitude,latitude,time,all_of(metrics_output)) %>% 
  st_as_stars(dims = c("longitude","latitude","time")) %>%
  st_set_crs("EPSG:4326") %>%
  st_warp(dest=st_as_stars(st_bbox(.), dx=0.1, method="med"))

# convert to ea_spatial
reasp <- as_ea_spatial(
  x = r,
  value_col = metrics_output,
  data_type    = data_types,
  units        = units,
  region       = "Northwest Atlantic",
  time_descriptor = "Annual",
  source_citation = "SOPhyE group, DFO",
  temporal_coverage = temporal_cov,
  variable_name = paste0(metrics_output,collapse=", ")
)

# Assign the object to a variable with the name from metric
assign("satellite_ocean_colour", reasp)

# Save the data with the metric name
do.call(usethis::use_data, list(as.name("satellite_ocean_colour"), overwrite = TRUE, internal = FALSE))


# # quick test - plotting t_start in 2007
# data(satellite_ocean_colour)
# times <- stars::st_get_dimension_values(satellite_ocean_colour@data, "time")
# plot(satellite_ocean_colour@data["t_start_value",,,which(times==lubridate::as_date("20070101"))])


# # ALTERNATIVE: SAVE EACH VARIABLE INDIVIDUALLY
# 
# for (metric in metrics_output) {
# 
#   # grab the metric info, data, and temporal coverage
#   metric_info <- metrics[[metric]]
#   tmp_df <- df %>% dplyr::select(longitude,latitude,time,all_of(metric))
#   temporal_cov <- paste0(year(range(tmp_df$time)),collapse="-")
# 
#   # convert the data for this metric into a stars object and reduce resolution to 0.1 degrees
#   r <- st_as_stars(tmp_df, dims = c("longitude","latitude","time")) %>%
#     st_set_crs("EPSG:4326") %>%
#     st_warp(dest=st_as_stars(st_bbox(.), dx=0.1, method="med"))
#   
#   # convert to ea_spatial
#   reasp <- as_ea_spatial(
#     x = r,
#     value_col = metric,
#     data_type    = metric_info$data_type,
#     units        = metric_info$units,
#     region       = "Northwest Atlantic",
#     time_descriptor = "Annual",
#     source_citation = "SOPhyE group, DFO",
#     temporal_coverage = temporal_cov,
#     variable_name = metric
#   )
# 
#   # Assign the object to a variable with the name from metric
#   assign(paste0("sophye_RR_",metric), reasp)
# 
#   # Save the data with the metric name
#   do.call(usethis::use_data, list(as.name(paste0("sophye_",metric)), overwrite = TRUE, internal = FALSE))
# 
# }

