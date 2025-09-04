#' Get citation information for marea
#'
#' Returns the recommended citation for the marea package, either as plain text or in BibTeX format.
#'
#' @param bibtex Logical. If TRUE, returns BibTeX format. If FALSE (default), returns plain text.
#' @return Citation information for the marea package.
#' @export
#'
#' @examples
#' # Get text citation
#' cite_marea()
#'
#' # Get BibTeX citation
#' cite_marea(bibtex = TRUE)
cite_marea <- function(bibtex = FALSE) {
  cit <- citation("marea")
  if (bibtex) {
    return(toBibtex(cit))
  } else {
    return(cit)
  }
}
