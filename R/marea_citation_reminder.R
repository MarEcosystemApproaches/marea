#' Print a reminder to cite marea
#'
#' Prints a message reminding users to cite the marea package in their work.
#'
#' @export
#'
#' @examples
#' marea_citation_reminder()
marea_citation_reminder <- function() {
  message("Thank you for using marea!")
  message("Please cite this package in your work:")
  message("")
  print(citation("marea"), bibtex = FALSE)
}