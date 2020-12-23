# Messages
message_successful_sign_in <-
  "Sign in was successful!"
message_successful_sign_out <-
  "Sign out was successful!"
message_successful_switched_to_site <-
  "Successfully switched to site:"

# Error messages
error_missing_credentials <-
  "Missing authorization credentials: Make sure to provide all required credentials!"
error_missing_parameters <-
  "Missing parameters: Make sure to provide all required parameters!"
error_is_not_a_url <-
  "Please provide a correct Tableau Server URL!"
error_not_signed_in <-
  "Not signed in to Tableau Server!"
error_already_signed_out <-
  "Already signed out!"

# API errors
error_api_response <- function(error_response) {
  error <-
    paste0(
      "[Status: ", error_response[["error_code"]], "] ",
      error_response[["error_summary"]], ": ",
      error_response[["error_detail"]]
    )
  return(error)
}
