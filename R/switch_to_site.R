#' @title Switch site
#' @description
#' Can be used to switch between sites on Tableau Server. This method is currently not available for Tableau Online.
#' The content URL is not the site name. The content URL is the value that in the server environment is referred to as
#' the site ID.
#' @param server_url Tableau Server URL
#' @param api_version The Tableau REST API version
#' @param api_token The API Access Token that was retrieved when signing in to Tableau Server
#' @param site_url The URL of the site to switch to
#' @return None.
#' @import httr XML
#' @export
#' @examples
#' \dontrun{
#' switch_to_site(
#'   server_url = Sys.getenv("TABLEAU_SERVER_URL"),
#'   api_version = Sys.getenv("TABLEAU_API_VERSION"),
#'   api_token = Sys.getenv("TABLEAU_API_TOKEN"),
#'   site_url = "MarketingSite")
#' }
switch_to_site <- function(server_url = Sys.getenv("TABLEAU_SERVER_URL"),
                           api_version = Sys.getenv("TABLEAU_API_VERSION"),
                           api_token = Sys.getenv("TABLEAU_API_TOKEN"),
                           site_url) {
  parameters <-
    list(
      server_url = server_url,
      api_version = api_version,
      api_token = api_token,
      site_url = site_url
    )

  if (is_null(parameters) || is_na(parameters) || missing(site_url))
    stop(error_missing_parameters)

  parameters <-
    parameters[-which(names(parameters) == "site_url")]

  if (is_blank(parameters))
    stop(error_not_signed_in)

  request_body <-
    create_switch_site_request_body(site = site_url)

  endpoint <- "/auth/switchSite"
  headers <- c('X-tableau-auth' = api_token)

  tableau_server_url <-
    compose_server_url(
      url = server_url,
      version = api_version,
      end_point = endpoint
    )

  response <-
    post_switch_site(
      tableau_server_url = tableau_server_url,
      request_body = request_body,
      headers = headers
    ) %>%
    check_for_api_error()

  if (response[["status"]] == "error")
    stop(error_api_response(response))

  site <-
    response$response$credentials$site$contentUrl

  message(paste(message_successful_switched_to_site, site))
}

create_switch_site_request_body <- function(site) {
  ts_request <-
    newXMLNode("tsRequest")
  newXMLNode(
    "site",
    attrs = c(contentUrl = site),
    parent = ts_request
  )

  request_body <-
    toString.XMLNode(ts_request)
  request_body <-
    trim_xml(request_body)

  return(request_body)
}

post_switch_site <- function(tableau_server_url, request_body, headers) {
  POST(url = tableau_server_url, body = request_body, add_headers(headers))
}
