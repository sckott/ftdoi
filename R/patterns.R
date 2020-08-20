#' Download patterns files
#' 
#' Does various checks to see if patterns files alrady downloaded,
#' out of date, if some/all are deleted and in need of an update
#' @export
#' @return character vector of file paths
#' @examples \dontrun{
#' ftd_fetch_patterns()
#' }
ftd_fetch_patterns <- function() {
  patterns_grab()
  ftdoi_cache$list()
}

latest_tag <- function() {
  rel_latest="https://api.github.com/repos/ropenscilabs/pubpatterns/releases/latest"
  jsonlite::fromJSON(ftd_GET(rel_latest))$tag_name
}
latest_tag_url <- function() {
  zip_url_pat=
  "https://github.com/ropenscilabs/pubpatterns/releases/download/%s/pubpatterns.zip"
  sprintf(zip_url_pat, latest_tag())
}
patterns_grab <- function() {
  zip_path <- file.path(ftdoi_cache$cache_path_get(), "pubpatterns.zip")
  tag <- file.path(ftdoi_cache$cache_path_get(), "tag.txt")
  manifest <- file.path(ftdoi_cache$cache_path_get(), "manifest.txt")
  lt <- latest_tag()
  if (needs_update(lt, tag, manifest)) { # if tags don't match
    url <- latest_tag_url()
    ftd_GET_zip(url)
    cat(lt, sep = "\n", file = tag)
    cat(grep("\\.json", basename(ftdoi_cache$list()), value = TRUE),
      sep = "\n", file = manifest)
    utils::unzip(zip_path, exdir = ftdoi_cache$cache_path_get())
    unlink(zip_path)
  }
}
needs_update <- function(lt, tag, manifest) {
  if (!dir.exists(ftdoi_cache$cache_path_get())) return(TRUE)
  if (!file.exists(tag)) return(TRUE)
  if (!file.exists(manifest)) return(TRUE)
  up2date <- lt == readLines(tag) &&
    all(sort(readLines(manifest)) %in% 
      sort(grep("\\.json", basename(ftdoi_cache$list()), value = TRUE)))
  if (up2date) {
    message("already up to date")
    return(FALSE)
  }
  return(TRUE)
}
