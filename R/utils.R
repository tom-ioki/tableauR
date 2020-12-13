compose_server_url <- function(url, version, end_point) {
  if (endsWith(url, "/")) {
    url <-
      substr(url, 1, nchar(url) - 1)
  }

  url <-
    paste0(url, "/api/", version, end_point)

  return(url)
}

check_for_api_error <- function(api_response) {
  response <- content(api_response)

  if (is.null(response$error)) {
    response <-
      list(
        status = "success",
        response = response
      )
    return(response)
  }

  error <- response$error
  response <-
    list(
      status = "error",
      error_code = error$code,
      error_summary = error$summary,
      error_detail = error$detail
    )
  return(response)
}
