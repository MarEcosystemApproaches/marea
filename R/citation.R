#' Get citation information for marea
#'
#' @param bibtex Logical. If TRUE, return BibTeX format
#' @return Citation information
#' @export
#' @examples
#' # Get text citation
#' cite_marea()
#' 
#' # Get BibTeX
#' cite_marea(bibtex = TRUE)
cite_marea <- function(bibtex = FALSE) {
  cit <- citation("marea")
  if (bibtex) {
    return(toBibtex(cit))
  } else {
    return(cit)
  }
}

#' Print citation reminder
#'
#' @export
marea_citation_reminder <- function() {
  message("Thank you for using marea!")
  message("Please cite this package in your work:")
  message("")
  print(citation("marea"), bibtex = FALSE)
}