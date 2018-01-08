#' DOI
#' 
#' @export
#' @param doi (character) a DOI. required
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' # found
#' pb_doi('10.7554/eLife.07404')
#' 
#' # not found
#' # pb_doi('10.1007/1-4020-0613-6_20780')
#' }
pb_doi <- function(doi, ...) {
  json_prx(pb_GET(sprintf("api/doi/%s/", doi), ...))
}
