#' @title Sign In to Tableau Server
#' @description
#' Sets the credentials and authenticate for a selected Tableau Server. The sign in function sets a secret access token
#' that can be used for all following API requests. This secret access token is valid for 240 minutes and has to be
#' refreshed after this time period.
#' @param token_name The name of the Tableau token
#' @param token_secret The Tableau personal access token
#' @param server_url Tableau Server URL
#' @param site_url The Site URL to log in (if left blank it logs into the default site)
#' @param api_version The Tableau REST API version
#' @return None.
#' @import XML httr
#' @export
#' @examples
#' \dontrun{
#' sign_in(
#'   token_name = "my_token",
#'   personal_access_token = "EE2D2915A4",
#'   server_url = "https://my-tableau-server.com",
#'   site_url = "my_site",
#'   api_version = 3.9)
#' }
sign_in <- function(token_name,
                    token_secret,
                    server_url,
                    site_url = "",
                    api_version) {
  parameters <-
    list(
      token_name = token_name,
      token_secret = token_secret,
      server_url = server_url,
      site_url = site_url,
      api_version = api_version
    )

  if (any(missing(token_name), missing(token_secret), missing(server_url), missing(api_version)) ||
      is_null(parameters) || is_na(parameters)) {
    stop(error_missing_credentials)
  }

  is_url(server_url)

  request_body <-
    create_request_body(
      personal_access_token_name = token_name,
      personal_access_token_secret = token_secret,
      sign_in_to_site = site_url
    )

  endpoint <- "/auth/signin"

  tableau_server_url <-
    compose_server_url(
      url = server_url,
      version = api_version,
      end_point = endpoint
    )

  response <-
    POST(url = tableau_server_url, body = request_body) %>%
    check_for_api_error()

  if (response[["status"]] == "error")
    stop(error_api_response(response))

  Sys.setenv(
    "TABLEAU_API_TOKEN" = response[["response"]]$credentials$token
  )

  Sys.setenv(
    "TABLEAU_API_VERSION" = api_version
  )

  Sys.setenv(
    "TABLEAU_SERVER_URL" = server_url
  )

  message(message_successful_sign_in)
}

create_request_body <- function(personal_access_token_name,
                                personal_access_token_secret,
                                sign_in_to_site) {
  attributes <-
    c(
      personalAccessTokenName = personal_access_token_name,
      personalAccessTokenSecret = personal_access_token_secret
    )

  ts_request <-
    newXMLNode("tsRequest")
  credentials <-
    newXMLNode(
      "credentials",
      attrs = attributes,
      parent = ts_request
    )
  newXMLNode(
    "site",
    attrs = c(contentUrl = sign_in_to_site),
    parent = credentials
  )

  request_body <-
    toString.XMLNode(ts_request)
  request_body <-
    gsub(">[[:space:]]+", ">", request_body)

  return(request_body)
}
