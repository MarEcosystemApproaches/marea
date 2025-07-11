#' Download Copernicus Marine (CMEMS) data
#'
#' Downloads data from the Copernicus Marine Environment Monitoring Service ([CMEMS](https://marine.copernicus.eu/)) using the Copernicus Marine API and the Python package `copernicusmarine`.
#'
#' @param username Your CMEMS username.
#' @param password Your CMEMS password.
#' @param dataset_id Dataset ID (default is "cmems_mod_glo_phy_my_0.083deg_P1M-m", the global ocean physics product).
#' @param variables Character vector of variable names to download.
#' @param minimum_longitude Minimum longitude for the data bounding box.
#' @param maximum_longitude Maximum longitude for the data bounding box.
#' @param minimum_latitude Minimum latitude for the data bounding box.
#' @param maximum_latitude Maximum latitude for the data bounding box.
#' @param start_datetime Start date and time (e.g., "1993-12-01T00:00:00").
#' @param end_datetime End date and time (e.g., "1994-12-01T00:00:00").
#' @param output_filename Name of the output file (NetCDF format).
#'
#' @return No return value. Downloads a NetCDF file to the specified location.
#' @importFrom reticulate use_virtualenv virtualenv_install virtualenv_create import
#' @export
#'
#' @examples
#' \dontrun{
#' get_CMEMS_ncdf(username = "your_username", password = "your_password", variables = c("thetao"))
#' }
get_CMEMS_ncdf <- function(username = NA, password = NA, dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m", variables, minimum_longitude = -67.74250, maximum_longitude = -54.90132, minimum_latitude = 40.04343, maximum_latitude = 47.83333, start_datetime = "1993-12-01T00:00:00", end_datetime = "1994-12-01T00:00:00", output_filename = tempfile(fileext = ".nc")) {
  # browser()
  
  pythonenv <- try(reticulate::use_virtualenv("CopernicusMarine", required = TRUE))
  
  if (inherits(pythonenv, "try-error")) {
    reticulate::virtualenv_create(envname = "CopernicusMarine")
    reticulate::virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
  }
  reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
  cmt <- try(import("copernicusmarine"))
  
  # Login function to create your configuration file
  if(!is.na(username)|!is.na(password)){
    cmt$login(username, password)
  }
  tmpdirnc <- tempdir()
  tmpnc <- tempfile(fileext = ".nc")
  cmt$subset(
    dataset_id=dataset_id,
    variables=variables,
    minimum_longitude=minimum_longitude,
    maximum_longitude=maximum_longitude,
    minimum_latitude=minimum_latitude,
    maximum_latitude=maximum_latitude,
    start_datetime=start_datetime,
    end_datetime=end_datetime,
    output_directory = dirname(output_filename),
    output_filename = basename(output_filename)
  )
  
}