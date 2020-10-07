#' prefix local
#' @export
#' @param doi (characte) a doi
#' @return a named list with: prefix, member, name
#' @examples
#' prefix_local('10.3390/ani4010082')
prefix_local <- function(doi) {
  prefix <- doi_prefix(doi)
  z <- crossref_member_prefix[crossref_member_prefix$prefixes %in% prefix,]
  list(prefix = prefix, member = as.character(z$id),
    name = z$primary_name)
}
