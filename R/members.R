#' Members
#' 
#' @export
#' @param id (character) a Crossref member ID. Default is `NULL` which
#' gets all members
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' df <- pb_members()
#' pb_members(221)
#' pb_members(1965)
#' 
#' # not found
#' # pb_members(999)
#' }
pb_members <- function(id = NULL, ...) {
  path <- if (is.null(id)) "api/members/" else sprintf("api/members/%s/", id)
  if (is.null(id)) {
    proc_many(pb_GET(path, ...))
  } else {
    json_prx(pb_GET(path, ...))
  }
}
