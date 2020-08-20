#' Members
#' 
#' @export
#' @param id (character) a Crossref member ID. Default is `NULL` which
#' gets all members
#' @examples \dontrun{
#' ftd_members()
#' ftd_members(221)
#' ftd_members(1965)
#' 
#' # not found
#' # ftd_members(999)
#' }
ftd_members <- function(id = NULL) {
  if (is.null(id)) all_members() else a_member(id)
}

all_members <- function(id) {
  paths <- vapply(member_map, "[[", "", "path")
  lapply(file.path(ftdoi_cache$cache_path_get(), paths), jsonlite::fromJSON)
}
a_member <- function(id) {
  x <- member_map[as.character(id)]
  if (is.null(x[[1]])) 
    stop('not a Crossref member or not supported yet', call.=FALSE)
  path <- file.path(ftdoi_cache$cache_path_get(), x[[1]]$path)
  if (!file.exists(path)) stop(paste(path, ' does not exist'), call.=FALSE)
  jsonlite::fromJSON(path)
}
