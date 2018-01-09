#' Prefixes
#' 
#' @export
#' @param id (character) a DOI prefix. Default is `NULL`, which 
#' gets all
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' ftd_prefixes()
#' ftd_prefixes(id = '10.1080')
#' ftd_prefixes(id = '10.1080', verbose = TRUE)
#' 
#' # doesn't work
#' # ftd_prefixes(id = '10.9999')
#' }
ftd_prefixes <- function(id = NULL, ...) {
  path <- if (is.null(id)) "api/prefixes/" else sprintf("api/prefixes/%s/", id)
  if (is.null(id)) {
    proc_many(ftd_GET(path, ...))
  } else {
    json_prx(ftd_GET(path, ...))
  }
}
