fxn_pub <- function(pub) publisher_funs[[pub]]

# individual publiser methods
pub_pensoft <- function(doi, pat, member, issn, res = NULL) {
  if (!is.null(issn)) doi_issn_update(doi, issn)
  if (is.null(issn)) issn <- doi_issn_get(doi)
  lks <- make_links(doi,
    Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]$urls,
    pat$journals[[1]]$components$doi$regex)
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = as.list(vapply(strsplit(issn, ",")[[1]], iurl, "")), links = lks)
}
pub_mdpi <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- lapply(res, function(w) {
      names(w) <- c("url","content-type")
      w
    })
  } else {
    lks <- list()
    for (i in seq_along(pat$urls)) {
      lks[[i]] <- list(
        url = sprintf(pat$urls[[i]], issn, res$volume, res$issue,
          strextract(doi, pat$components$doi$regex)),
        'content-type' = get_ctype(names(pat$urls)[i])
      )
    }
    for (i in seq_along(lks)) {
      url_update(doi, lks[[i]]$url, lks[[i]]$`content-type` %||% "")
    }
  }
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
pub_pnas <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- list(list(url = res$link[[1]]$URL,
      'content-type' = res$link[[1]]$content_type))
  } else {
    lks <- list()
    for (i in seq_along(pat$urls)) {
      lks[[i]] <- list(
        url = sprintf(pat$urls[[i]], res$volume, res$issue,
          sub("E", "", strsplit(res$page, "-")[[1]][1])),
        'content-type' = get_ctype(names(pat$urls)[i])
      )
    }
    url_update(doi, lks[[1]]$url, lks[[1]]$`content-type` %||% "")
  }
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
first_page <- function(x) strsplit(x, "-")[[1]][1]
pub_company_of_biologists <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- list(list(url = res$link[[1]]$URL,
      'content-type' = res$link[[1]]$content_type))
  } else {
    z <- Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]
    lks <- list(list(
      url = sprintf(z$urls$pdf, res$volume, res$issue, first_page(res$page)),
      'content-type' = get_ctype(names(z$urls)[1])
    ))
    url_update(doi, lks[[1]]$url, lks[[1]]$`content-type` %||% "")
  }
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
pub_iif <- function(doi, pat, member, issn, res=NULL) {
  lks <- list(
    url = sub("view", "download", res$link[[1]]$URL),
    'content-type' = get_ctype(names(pat$urls)[1])
  )
  url_update(doi, lks$url, lks$`content-type` %||% "")
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}

# factories
pub_factory1 <- function() {
  function(doi, pat, member, issn, res=NULL) {
    lks <- make_links(doi, pat$urls, pat$components$doi$regex)
    list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
      issn = iurl(issn), links = lks)
  }
}
pub_factory2 <- function() {
  function(doi, pat, member, issn, res=NULL) {
    if (!is.null(issn)) doi_issn_update(doi, issn)
    if (is.null(issn)) issn <- doi_issn_get(doi)
    lks <- make_links(doi,
      Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]$urls,
      pat$journals[[1]]$components$doi$regex)
    list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
      issn = as.list(vapply(strsplit(issn, ",")[[1]], iurl, "")), links = lks)
  }
}
pub_factory3 <- function() {
  function(doi, pat, member, issn, res) {
    tmp <- res$link[[1]]
    lks <- list(
      url = tmp[tmp$intended.application == "similarity-checking", ]$URL,
      'content-type' = names(pat$urls)
    )
    url_update(doi, lks$url, lks$`content-type` %||% "")
    list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
        issn = iurl(issn), links = list(lks))
  }
}
pub_frontiers <- pub_informa <- pub_elife <- pub_emerald <-
  pub_pleiades <- pub_sage <- pub_spie <- pub_springer <- 
  pub_american_society_of_clinical_oncology <- pub_aip <- 
  pub_acs <- pub_the_royal_society <- pub_factory1()
pub_plos <- pub_thieme <- pub_peerj <- pub_factory2()
pub_aps <- pub_rsc <- pub_karger <- pub_transtech <- pub_factory3()
publisher_funs <- list(
  elife = pub_elife,
  pensoft = pub_pensoft,
  plos = pub_plos,
  mdpi = pub_mdpi,
  frontiers = pub_frontiers,
  informa = pub_informa,
  thieme = pub_thieme,
  peerj = pub_peerj,
  aps = pub_aps,
  rsc = pub_rsc,
  karger = pub_karger,
  transtech = pub_transtech,
  emerald = pub_emerald,
  pleiades = pub_pleiades,
  iif = pub_iif,
  sage = pub_sage,
  spie = pub_spie,
  pnas = pub_pnas,
  springer = pub_springer,
  american_society_of_clinical_oncology = pub_american_society_of_clinical_oncology,
  aip = pub_aip,
  acs = pub_acs,
  the_royal_society = pub_the_royal_society,
  company_of_biologists = pub_company_of_biologists
)
