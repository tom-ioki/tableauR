# Messages
message_successful_login <-
  "Login was successful!"

# Error messages
error_missing_credentials <-
  "Missing authorization credentials: Make sure to provide all required credentials!"
error_is_not_a_url <-
  "Please provide a correct Tableau Server URL!"

# Api errors
error_api_response <- function(error_response) {
  error <-
    paste0(
      "[Status: ", error_response[["error_code"]], "] ",
      error_response[["error_summary"]], ": ",
      error_response[["error_detail"]]
    )
  return(error)
}
