#' Heartbeat
#' 
#' @export
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' ftd_heartbeat()
#' }
ftd_heartbeat <- function(...) {
  json_parse(ftd_GET("api/heartbeat/", ...))
}
