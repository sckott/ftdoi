#' @title ftdoi
#' @description Interface to the ftdoi.org API for publisher url patterns.
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @importFrom hoardr hoard
#' @importFrom utils unzip
#' @importFrom storr storr_rds
#' @importFrom rcrossref cr_works
#' @importFrom data.table rbindlist setDF
#' @importFrom xml2 read_xml
#' @name ftdoi-package
#' @aliases ftdoi
#' @docType package
#' @keywords package
NULL

#' Local cache of metadata on Crossref members
#'
#' @format A data frame with 576 rows and 5 variables
#' 
#' @details with columns:
#' 
#' - id: the Crossref publisher member identifier
#' - prefixes: DOI prefixes for the publisher
#' - primary_name: the Crossref publisher member name
#'
#' @name crossref_member_prefix
#' @docType data
#' @keywords data
NULL
