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
pub_hindawi <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- list(list(url = res$link[[1]]$URL,
      'content-type' = res$link[[1]]$content_type))
  } else {
    links <- res$link[[1]]
    use <- links[links$content.type == "application/pdf",]
    lks <- list(
      url = use$URL,
      'content-type' = use$content.type
    )
  }
  url_update(doi, lks$url, lks$`content-type` %||% "")
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
pub_aaas <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- list(list(url = res$link[[1]]$URL,
      'content-type' = res$link[[1]]$content_type))
  } else {
    lks <- list(url = fat_cat_link(doi), 'content-type' = "pdf")
    if (is.na(lks$url)) {
      z <- Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]
      if (grepl("2375-2548", issn)) {
        last_part = strextract(doi, z$components$doi$regex, perl=TRUE)
      } else {
        last_part = strsplit(res$page, "-")[[1]][1]
      }
      url = sprintf(z$urls$pdf, res$volume, res$issue, last_part)
      lks <- list(url = url, 'content-type' = "pdf")
    }
  }
  url_update(doi, lks$url, lks$`content-type` %||% "")
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
pub_cdc <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- list(list(url = res$link[[1]]$URL,
      'content-type' = res$link[[1]]$content_type))
  } else {
    issn <- res$issn
    year <- strsplit(res$created, "-")[[1]][1]
    doi_part <- last(strsplit(last(strsplit(doi, '/')[[1]]), '\\.')[[1]])
    last_part <- paste(substring(doi_part, 1, 2),
      substring(doi_part, 3, nchar(doi_part)), sep="_")
    url <- sprintf(pat$urls$pdf, year, last_part)
    lks <- list(url = url, 'content-type' = "pdf")
  }
  url_update(doi, lks$url, lks$`content-type` %||% "")
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
pub_elsevier <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- links2df(res$link[[1]])
  } else {
    issn <- res$issn
    lks <- make_links_no_regex(res$link[[1]]$URL, res$link[[1]]$content.type)
    for (i in seq_along(lks)) {
      url_update(doi, lks[[i]]$url, lks[[i]]$`content-type` %||% "")
    }
    # FIXME: see alternative-id bit in pubpatternsapi
  }
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
pub_american_society_for_microbiology <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- links2df(res$link[[1]])
  } else {
    issn <- res$issn
    url <- pat$urls$pdf
    vol <- res$volume
    iss <- res$issue
    journal_bit <- tolower(strextract(doi, "[A-Za-z]+"))
    other_bit <- strextract(strsplit(doi, "/")[[1]][2], "[0-9]+-[0-9]+")
    lk <- sprintf(url, journal_bit, vol, iss, other_bit)
    lks <- list(url = lk, 'content-type' = "pdf")
    url_update(doi, lks$url, lks$`content-type`)
  }
  list(doi = doi, member = list(name = pat$publisher, url = murl(member)),
    issn = iurl(issn), links = lks)
}
pub_de_gruyter <- function(doi, pat, member, issn, res=NULL) {
  if (all("link" == names(res))) {
    lks <- links2df(res$link[[1]])
  } else {
    url <- res$link[[1]][res$link[[1]]$intended.application == "similarity-checking", ]$URL
    if (!grepl("view", url)) {
      url <- sub("\\.xml", "\\.pdf", url)
    } else {
      base <- strextract(dirname(url), "https?://[A-Za-z.]+")
      base <- file.path(base, "downloadpdf/journals")
      vol <- res$volume
      iss <- res$issue
      jabbrev <- strextract(last(strsplit(doi, "/")[[1]]), "[A-Za-z]+")
      page <- res$page
      if (is.null(page)) {
        url <- NULL
      } else {
        last_part <- sprintf("article-p%s.pdf", page)
        url <- file.path(base, jabbrev, vol, iss, last_part)
      }
    }
    lks <- list(url = url, 'content-type' = "pdf")
    url_update(doi, lks$url, lks$`content-type`)
  }
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
  pub_acs <- pub_the_royal_society <- pub_iop <- pub_factory1()
pub_plos <- pub_thieme <- pub_peerj <- pub_factory2()
pub_aps <- pub_rsc <- pub_karger <- pub_transtech <-
  pub_oxford <- pub_factory3()
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
  hindawi = pub_hindawi,
  iop = pub_iop,
  company_of_biologists = pub_company_of_biologists,
  aaas = pub_aaas,
  oxford = pub_oxford,
  cdc = pub_cdc,
  elsevier = pub_elsevier,
  american_society_for_microbiology = pub_american_society_for_microbiology,
  de_gruyter = pub_de_gruyter
)
