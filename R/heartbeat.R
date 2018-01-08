#' Heartbeat
#' 
#' @export
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples \dontrun{
#' pb_heartbeat()
#' }
pb_heartbeat <- function(...) {
  json_parse(pb_GET("api/heartbeat/", ...))
}
