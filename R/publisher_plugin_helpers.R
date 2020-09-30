members_need_url <- c("16", "292", "127", "2457", "341", "237", "1968", "8215")
members_need_issn <- c("2258", "340", "194", "4443")
members_sim_check <- c("16", "292", "127", "2457")
pattern_path <- function(id) {
  file.path(ftdoi_cache$cache_path_get(), member_map[[id]]$path)
}
pattern_member <- function(doi, member, issn, res = NULL) {
  pub <- switch(member,
    "4374" = "elife",
    "2258" = "pensoft",
    "340" = "plos",
    "1968" = "mdpi",
    "1965" = "frontiers",
    "301" = "informa",
    "194" = "thieme",
    "4443" = "peerj",
    "16" = "aps",
    "292" = "rsc",
    "127" = "karger",
    "2457" = "transtech",
    "140" = "emerald",
    "137" = "pleiades",
    "8215" = "iif",
    "179" = "sage",
    "189" = "spie",
    "341" = "pnas",
    "297" = "springer",
    "233" = "american_society_of_clinical_oncology",
    "317" = "aip",
    "316" = "acs",
    "175" = "the_royal_society",
    "237" = "company_of_biologists"
  )
  pat <- jsonlite::fromJSON(pattern_path(member), FALSE)
  fxn_pub(pub)(doi, pat, member, issn, res)
}

get_ctype <- function(x) {
  switch(x,
    pdf = 'application/pdf',
    xml = 'application/xml',
    html = 'text/html'
  )
}
murl <- function(x) file.path("https://api.crossref.org/members", x)
iurl <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  x <- strsplit(x, ",")[[1]][1]
  file.path("https://api.crossref.org/journals", x)
}
make_links <- function(doi, z, regex) {
  out = list()
  for (i in seq_along(z)) {
    out[[i]] <- list(
      url = sprintf(z[[i]], strextract(doi, regex)),
      'content-type' = get_ctype(names(z)[i])
    )
  }
  return(out)
}
