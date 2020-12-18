context("Sign out")

describe("Validation of function parameters", {
  url = "https://my-tableau-server.com"
  version = 3.9
  token = "FOOBAR"

  expected_error_message <-
    "Missing authorization credentials: Make sure to provide all required credentials!"

  describe("when one of the function parameters is NULL", {
    describe("server_url", {
      it("returns an error", {
        expect_error(
          sign_out(server_url = NULL, api_version = version, api_token = token),
          expected_error_message
        )
      })
    })

    describe("api_version", {
      it("returns an error", {
        expect_error(
          sign_out(server_url = url, api_version = NULL, api_token = token),
          expected_error_message
        )
      })
    })

    describe("api_token", {
      it("returns an error", {
        expect_error(
          sign_out(server_url = url, api_version = version, api_token = NULL),
          expected_error_message
        )
      })
    })
  })

  describe("when one of the function parameters is NA", {
    describe("server_url", {
      it("returns an error", {
        expect_error(
          sign_out(server_url = NA, api_version = version, api_token = token),
          expected_error_message
        )
      })
    })

    describe("api_version", {
      it("returns an error", {
        expect_error(
          sign_out(server_url = url, api_version = NA, api_token = token),
          expected_error_message
        )
      })
    })

    describe("api_token", {
      it("returns an error", {
        expect_error(
          sign_out(server_url = url, api_version = version, api_token = NA),
          expected_error_message
        )
      })
    })
  })
})
