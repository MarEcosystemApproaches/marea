#' Get CMEMS data
#'
#' @param username character for your CMEMS username
#' @param password character for your CMEMS password
#' @param dataset_id character string with the dataset id
#' @param variables character vector with variable names
#' @param minimum_longitude minimum longitude
#' @param maximum_longitude maximum longitude
#' @param minimum_latitude minimum latitude
#' @param maximum_latitude maximum latitude
#' @param start_datetime character string for the start date and time e.g. "1993-12-01T00:00:00"
#' @param end_datetime character string for the end date and time e.g. "1994-12-01T00:00:00"
#' @param output_filename character string for the output filename
#'
#' @returns
#' @importFrom reticulate use_virtualenv virtualenv_install virtualenv_create
#' @export
#'
#' @examples
#'
get_CMEMS_ncdf <- function(username = NA, password = NA, dataset_id, variables, minimum_longitude, maximum_longitude, minimum_latitude, maximum_latitude, start_datetime, end_datetime, output_filename = tempfile(fileext = ".nc")) {
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
  } else {
    stop("Please provide a username and password")
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




