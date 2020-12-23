#' @title Sign Out from Tableau Server
#' @description
#' Unsets the credentials and signs out from a selected Tableau Server.
#' @param server_url Tableau Server URL
#' @param api_version The Tableau REST API version
#' @param api_token The API Access Token that was retrieved when signing in to Tableau Server
#' @return None.
#' @import httr
#' @export
#' @examples
#' \dontrun{
#' sign_out(
#'   server_url = Sys.getenv("TABLEAU_SERVER_URL"),
#'   api_version = Sys.getenv("TABLEAU_API_VERSION"),
#'   api_token = Sys.getenv("TABLEAU_API_TOKEN"))
#' }
sign_out <- function(server_url = Sys.getenv("TABLEAU_SERVER_URL"),
                     api_version = Sys.getenv("TABLEAU_API_VERSION"),
                     api_token = Sys.getenv("TABLEAU_API_TOKEN")) {
  parameters <-
    list(
      server_url = server_url,
      api_version = api_version,
      api_token = api_token
    )

  if (is_null(parameters) || is_na(parameters))
    stop(error_missing_credentials)

  if (is_blank(parameters))
    stop(error_already_signed_out)

  endpoint <- "/auth/signout"
  headers <- c('X-tableau-auth' = api_token)

  tableau_server_url <-
    compose_server_url(
      url = server_url,
      version = api_version,
      end_point = endpoint
    )

  response <-
    post_sign_out(tableau_server_url = tableau_server_url, headers = headers) %>%
    check_for_api_error()

  if (response[["status"]] == "error")
    stop(error_api_response(response))

  Sys.unsetenv("TABLEAU_SERVER_URL")
  Sys.unsetenv("TABLEAU_API_VERSION")
  Sys.unsetenv("TABLEAU_API_TOKEN")

  message(message_successful_sign_out)
}

post_sign_out <- function(tableau_server_url, headers) {
  POST(url = tableau_server_url, add_headers(headers))
}
