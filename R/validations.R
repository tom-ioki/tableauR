is_url <- function(url) {
  regex <-
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  is_url <-
    grepl(regex, url)
  if (!is_url)
    stop(error_is_not_a_url)
}

is_null <- function(parameters) {
  any(
    unlist(
      lapply(parameters, is.null)
    )
  )
}

is_na <- function(parameters) {
  any(
    unlist(
      lapply(parameters, is.na)
    )
  )
}