compose_server_url <- function(url, version, end_point) {
  if (endsWith(url, "/")) {
    url <-
      substr(url, 1, nchar(url) - 1)
  }

  url <-
    paste0(url, "/api/", version, end_point)

  return(url)
}

trim_xml <- function(xml) {
  gsub(">[[:space:]]+", ">", xml)
}
