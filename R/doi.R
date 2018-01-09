#' DOI
#' 
#' @export
#' @param doi (character) a DOI. required
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' # found
#' ftd_doi('10.7554/eLife.07404')
#' 
#' # not found
#' # ftd_doi('10.1007/1-4020-0613-6_20780')
#' }
ftd_doi <- function(doi, ...) {
  json_prx(ftd_GET(sprintf("api/doi/%s/", doi), ...))
}
