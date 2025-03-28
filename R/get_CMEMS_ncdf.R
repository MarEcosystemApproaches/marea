#' Get CMEMS data
#'
#' This function downloads Copernicus Marine Environment Monitoring Service ([CMEMS](https://marine.copernicus.eu/)) data using the Copernicus Marine API and the python package copernicusmarine.
#'
#' @param username character for your CMEMS username
#' @param password character for your CMEMS password
#' @param dataset_id character string with the dataset id. Defaults to "cmems_mod_glo_phy_my_0.083deg_P1M-m" which is the global ocean physics analysis and forecast product on a 1/12° grid. Additional products can be found in the [Copernicus Data Store](https://data.marine.copernicus.eu/products)
#' @param variables character vector with variable names
#' @param minimum_longitude numeric minimum longitude of a bounding box for the data download. Defaults to -67.74250 which is for the Scotian Shelf and Bay of Fundy planning area.
#' @param maximum_longitude numeric maximum longitude of a bounding box for the data download. Defaults to -54.90132 which is for the Scotian Shelf and Bay of Fundy planning area.
#' @param minimum_latitude numeric minimum latitude of a bounding box for the data download. Defaults to 40.04343 which is for the Scotian Shelf and Bay of Fundy planning area.
#' @param maximum_latitude numeric maximum latitude of a bounding box for the data download. Defaults to 47.83333 which is for the Scotian Shelf and Bay of Fundy planning area.
#' @param start_datetime character string for the start date and time e.g. "1993-12-01T00:00:00"
#' @param end_datetime character string for the end date and time e.g. "1994-12-01T00:00:00"
#' @param output_filename character string for the output filename
#'
#' @returns
#' @importFrom reticulate use_virtualenv virtualenv_install virtualenv_create import
#' @export
#'
#' @examples
#'
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




