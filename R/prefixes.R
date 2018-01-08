#' Prefixes
#' 
#' @export
#' @param id (character) a DOI prefix. Default is `NULL`, which 
#' gets all
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' pb_prefixes()
#' pb_prefixes(id = '10.1080')
#' pb_prefixes(id = '10.1080', verbose = TRUE)
#' 
#' # doesn't work
#' # pb_prefixes(id = '10.9999')
#' }
pb_prefixes <- function(id = NULL, ...) {
  path <- if (is.null(id)) "api/prefixes/" else sprintf("api/prefixes/%s/", id)
  if (is.null(id)) {
    proc_many(pb_GET(path, ...))
  } else {
    json_prx(pb_GET(path, ...))
  }
}
