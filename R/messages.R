# Messages
message_successful_sign_in <-
  "Sign in was successful!"

message_successful_sign_out <-
  "Sign out was successful!"

# Error messages
error_missing_credentials <-
  "Missing authorization credentials: Make sure to provide all required credentials!"
error_is_not_a_url <-
  "Please provide a correct Tableau Server URL!"
error_already_signed_out <-
  "Already signed out!"

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
