context("Messages")

describe("Messages", {
  describe("Sign in", {
    it("returns the correct message", {
      expect_equal(
        message_successful_sign_in,
        "Sign in was successful!"
      )
    })
  })

  describe("Sign out", {
    it("returns the correct message", {
      expect_equal(
        message_successful_sign_out,
        "Sign out was successful!"
      )
    })
  })

  describe("Switch to site", {
    it("returns the correct message", {
      expect_equal(
        message_successful_switched_to_site,
        "Successfully switched to site:"
      )
    })
  })
})

describe("Error messages", {
  describe("'Missing credentials", {
    it("returns the correct error message", {
      expect_equal(
        error_missing_credentials,
        "Missing authorization credentials: Make sure to provide all required credentials!"
      )
    })
  })

  describe("'Missing parameters", {
    it("returns the correct error message", {
      expect_equal(
        error_missing_parameters,
        "Missing parameters: Make sure to provide all required parameters!"
      )
    })
  })

  describe("Not a URL", {
    it("returns the correct error message", {
      expect_equal(
        error_is_not_a_url,
        "Please provide a correct Tableau Server URL!"
      )
    })
  })

  describe("Not signed in", {
    it("returns the correct error message", {
      expect_equal(
        error_not_signed_in,
        "Not signed in to Tableau Server!"
      )
    })
  })

  describe("User already signed out", {
    it("returns the correct error message", {
      expect_equal(
        error_already_signed_out,
        "Already signed out!"
      )
    })
  })
})

describe("API errors", {
  describe("API error message", {
    response <-
      list(
        status = "error",
        error_code = "401001",
        error_summary = "Signin Error",
        error_detail = "Error signing in to Tableau Server"
      )

    expected_message <-
      "[Status: 401001] Signin Error: Error signing in to Tableau Server"

    it("returns the correct error message", {
      expect_equal(
        error_api_response(error_response = response),
        expected_message
      )
    })
  })
})
