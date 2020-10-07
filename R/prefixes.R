#' Prefixes
#' 
#' @export
#' @param id (character) a DOI prefix. Default is `NULL`, which 
#' gets all
#' @return named list of details of the publisher for the DOI prefix
#' @examples
#' ftdoi_cache$cache_path_set(path="foo", type="tempdir")
#' ftdoi_cache
#' ftd_fetch_patterns()
#' z <- list.files(file.path(ftdoi_cache$cache_path_get(), "patterns"))
#' if (length(z)) {
#'   ftd_prefixes(id = '10.1080')
#' }
#' 
#' \dontrun{
#' ftd_prefixes()
#' ftd_prefixes(id = '10.1080')
#' 
#' # doesn't work
#' # ftd_prefixes(id = '10.9999')
#' }
ftd_prefixes <- function(id = NULL) {
  assert(id, "character")
  if (is.null(id)) all_prefixes() else a_prefix(id)
}

prefix_map <- list(
  "10.1080" = list(
    "name" = "cogent",
    "path" = "cogent.json"
  ),
  "10.2139" = list(
    "name" = "ssrn",
    "path" = "ssrn.json"
  )
)
all_prefixes <- function(id) {
  paths <- vapply(prefix_map, "[[", "", "path")
  lapply(file.path(ftdoi_cache$cache_path_get(), "patterns", paths),
    jsonlite::fromJSON)
}
a_prefix <- function(id) {
  x <- prefix_map[as.character(id)]
  if (is.null(x[[1]])) 
    stop('not a DOI prefix or not supported yet', call.=FALSE)
  path <- file.path(ftdoi_cache$cache_path_get(), "patterns", x[[1]]$path)
  if (!file.exists(path)) stop(paste(path, ' does not exist'), call.=FALSE)
  jsonlite::fromJSON(path)
}
