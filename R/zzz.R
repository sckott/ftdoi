if (base::getRversion() >= "2.15.1") {
  utils::globalVariables(c("crossref_member_prefix"))
}

char2num <- function(x) as.numeric(strextract(x, "[0-9]+"))
strextract <- function(str, pattern, ...) regmatches(str, regexpr(pattern, str, ...))
strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)
pop <- function(x, nms) x[!names(x) %in% nms]
vc <- function(x) Filter(Negate(is.null), x)
assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}
`%||%` <- function (x, y) if (is.null(x) || is.na(x)) y else x
no_http_needed <- function(x) !x$member %in% members_need_url
prefix_local <- function(doi) {
  prefix <- doi_prefix(doi)
  id <- crossref_member_prefix[crossref_member_prefix$prefixes %in% prefix,]$id
  list(prefix = prefix, member = as.character(id))
}
make_doi_str <- function(x) {
  sprintf("doi:(\"%s\")", paste0(x, collapse = "\" OR \""))
}
fat_cat_link <- function(doi) {
  cn <- crul::HttpClient$new("https://search.fatcat.wiki")
  query <- list(q = make_doi_str(doi), size = 1)
  res <- cn$get("fatcat_release/_search", query = query)
  res$raise_for_status()
  out <- jsonlite::fromJSON(res$parse("UTF-8"), flatten = TRUE)$hits$hits
  out$`_source.best_pdf_url`
}
last <- function(x) x[length(x)]
pluck <- function(x, y, type = "") {
  unname(vapply(x, "[[", type, y))
}
links2df <- function(x) {
  # lks <- apply(x, 1, as.list)
  # lapply(lks, function(w) stats::setNames(w, c("url", "content_type")))
  stats::setNames(x, c("url","content_type"))
}
first_page <- function(x) strsplit(x, "-")[[1]][1]
fxn_pub <- function(pub) publisher_funs[[pub]]
to_df <- function(doi, pat, member, issn, lks) {
  data.frame(doi = doi, lks, issn = issn %||% NA_character_,
    member_name = pat$publisher, member_url = murl(member))
}
is_doi <- function(x) grepl("[0-9]+\\.[0-9]+/.+", x)
check_dois <- function(x) {
  stopifnot(inherits(x, "list") || inherits(x, "character"))
  x <- vapply(x, utils::URLdecode, "")
  res <- vapply(x, is_doi, logical(1))
  if (all(res)) return(TRUE)
  stop("These are probably not DOIs:\n\n", paste0(names(res[!res]), "\n"),
    call. = FALSE)
}
rbl <- function(x) {
  (data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE)
  ))
}
