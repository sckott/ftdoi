ftdoi_cache <- NULL # nocov start
ftdoi_doi_prefixes <- ftdoi_urls <- ftdoi_doi_issn <- NULL
.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set("ftdoi")
  ftdoi_cache <<- x

  # create storr's
  ftdoi_doi_prefixes <<- storr::storr_rds(
    file.path(x$cache_path_get(), "_doi_prefixes"),
    mangle_key = TRUE)
  ftdoi_urls <<- storr::storr_rds(
    file.path(x$cache_path_get(), "_urls"),
    mangle_key = TRUE)
  ftdoi_doi_issn <<- storr::storr_rds(
    file.path(x$cache_path_get(), "_doi_issn"),
    mangle_key = TRUE)
} # nocov end
