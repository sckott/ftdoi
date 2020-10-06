#' prefix local
#' @export
#' @param doi (characte) a doi
#' @return a named list with: prefix, member, name
prefix_local <- function(doi) {
  prefix <- doi_prefix(doi)
  z <- crossref_member_prefix[crossref_member_prefix$prefixes %in% prefix,]
  list(prefix = prefix, member = as.character(z$id),
    name = z$primary_name)
}
