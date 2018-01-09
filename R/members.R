#' Members
#' 
#' @export
#' @param id (character) a Crossref member ID. Default is `NULL` which
#' gets all members
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' df <- ftd_members()
#' ftd_members(221)
#' ftd_members(1965)
#' 
#' # not found
#' # ftd_members(999)
#' }
ftd_members <- function(id = NULL, ...) {
  path <- if (is.null(id)) "api/members/" else sprintf("api/members/%s/", id)
  if (is.null(id)) {
    proc_many(ftd_GET(path, ...))
  } else {
    json_prx(ftd_GET(path, ...))
  }
}
