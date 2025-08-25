#' Download Copernicus Marine (CMEMS) data
#'
#' Downloads data from the Copernicus Marine Environment Monitoring Service ([CMEMS](https://marine.copernicus.eu/)) using the Copernicus Marine API and the Python package `copernicusmarine`.
#'
#' @param username Your CMEMS username.
#' @param password Your CMEMS password.
#' @param dataset_id Dataset ID (default is "cmems_mod_glo_phy_my_0.083deg_P1M-m", the global ocean physics product).
#' @param variables list of variable names to download.
#' @param minimum_longitude Minimum longitude for the data bounding box.
#' @param maximum_longitude Maximum longitude for the data bounding box.
#' @param minimum_latitude Minimum latitude for the data bounding box.
#' @param maximum_latitude Maximum latitude for the data bounding box.
#' @param start_datetime Start date and time (e.g., "1993-12-01T00:00:00").
#' @param end_datetime End date and time (e.g., "1994-12-01T00:00:00").
#' @param output_filename Name of the output file (NetCDF format).
#' @param output Type of output: download on the "file" (default), or load as an object using "stars", or "terra" packages.
#' @param ... Additional arguments passed to the `stars::read_ncdf` function or `terra::rast` for loading the data.
#'
#' @return No return value. Downloads a NetCDF file to the specified location.
#' @importFrom reticulate use_virtualenv virtualenv_install virtualenv_create import
#' @export
#'
#' @examples
#' \dontrun{
#' get_CMEMS_ncdf(username = "your_username", password = "your_password", variables = list("thetao"))
#' }
get_CMEMS_ncdf <- function(username = Sys.getenv("COPERNICUS_USERNAME"),
                           password = Sys.getenv("COPERNICUS_PASSWORD"),
                           dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
                           variables, minimum_longitude = -67.74250,
                           maximum_longitude = -54.90132,
                           minimum_latitude = 40.04343,
                           maximum_latitude = 47.83333,
                           start_datetime = "1993-12-01T00:00:00",
                           end_datetime = "1994-12-01T00:00:00",
                           output_filename = tempfile(fileext = ".nc"),
                           output = "file",
                           ...) {

  pythonenv <- try(reticulate::use_virtualenv("CopernicusMarine", required = TRUE))
  
  if (inherits(pythonenv, "try-error")) {
    reticulate::virtualenv_create(envname = "CopernicusMarine")
    reticulate::virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))
  }
  reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
  cmt <- try(import("copernicusmarine"))
  
  # Login function to create your configuration file
  if (nzchar(username) && nzchar(password)) {
    cmt$login(username, password, force_overwrite = TRUE)
  } else {
    stop("Please supply username and password arguments, 
    use Sys.setenv() to set COPERNICUS_PASSWORD and COPERNICUS_USERNAME,
    or set them in your .Renviron file.")
  }

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
  
  if (output == "file") {
    message("Data downloaded to: ", output_filename)
  } else if (output == "stars") {
    nc_data <- stars::read_ncdf(output_filename, var = variables, ...)
    message("Data downloaded to: ", output_filename, "and loaded as stars object.")
    return(nc_data)
  } else if (output == "terra") {
    nc_data <- terra::rast(output_filename, ...)
    message("Data downloaded to: ", output_filename, "and loaded as terra object.")
    return(nc_data)
  } else {
    stop("Invalid output type. Use 'file', 'stars', or 'terra'.")
  }
  
}