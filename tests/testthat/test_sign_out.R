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

  describe("when User has already signed out", {
    Sys.unsetenv("TABLEAU_API_VERSION")
    Sys.unsetenv("TABLEAU_SERVER_URL")
    Sys.unsetenv("TABLEAU_API_TOKEN")

    it("returns an error", {
      expect_error(
        sign_out(),
        "Already signed out!"
      )
    })
  })
})

describe("#sign_out", {
  url <- "https://www.my-tableau-server.com"
  version <- "3.9"
  token <- "FOOBAR"

  example_url <-
    "https://www.my-tableau-server.com/api/3.9/auth/signout"

  example_headers <-
    c('X-tableau-auth' = token)

  describe("when the sign out was successful", {
    Sys.setenv("TABLEAU_API_VERSION" = version)
    Sys.setenv("TABLEAU_SERVER_URL" = url)
    Sys.setenv("TABLEAU_API_TOKEN" = token)

    response <-
      list(
        status = "success",
        response = as.raw(NULL)
      )

    post_sign_out_stub <- stubthat::stub(post_sign_out)
    post_sign_out_stub$withArgs(tableau_server_url = example_url, headers = example_headers)$returns(response)
    check_for_api_error_stub <- stubthat::stub(check_for_api_error)
    check_for_api_error_stub$withArgs(api_response = response)$returns(response)

    sign_out_double <- function(url, version, token) {
      mockr::with_mock(post_sign_out = post_sign_out_stub$f,
                       check_for_api_error = check_for_api_error_stub$f,
                       sign_out(server_url = url, api_version = version, api_token = token))
    }

    it("returns a confirmation message", {
      expect_message(
        sign_out_double(url = url, version = version, token = token),
        "Sign out was successful!"
      )
    })

    it("unsets the environment variables", {
      expect_equal(
        Sys.getenv("TABLEAU_API_TOKEN"),
        ""
      )
      expect_equal(
        Sys.getenv("TABLEAU_SERVER_URL"),
        ""
      )
      expect_equal(
        Sys.getenv("TABLEAU_API_VERSION"),
        ""
      )
    })
  })

  describe("when the sign out failed", {
    Sys.setenv("TABLEAU_API_VERSION" = version)
    Sys.setenv("TABLEAU_SERVER_URL" = url)
    Sys.setenv("TABLEAU_API_TOKEN" = token)

    response <-
      list(
        status = "error",
        error_code = "401002",
        error_summary = "Unauthorized Access",
        error_detail = "Invalid authentication credentials were provided."
      )

    post_sign_out_stub <- stubthat::stub(post_sign_out)
    post_sign_out_stub$withArgs(tableau_server_url = example_url, headers = example_headers)$returns(response)
    check_for_api_error_stub <- stubthat::stub(check_for_api_error)
    check_for_api_error_stub$withArgs(api_response = response)$returns(response)

    sign_out_double <- function(url, version, token) {
      mockr::with_mock(post_sign_out = post_sign_out_stub$f,
                       check_for_api_error = check_for_api_error_stub$f,
                       sign_out(server_url = url, api_version = version, api_token = token))
    }

    it("returns an error message", {
      expect_error(
        sign_out_double(url = url, version = version, token = token)
      )
    })

    it("does not unset the environment varialbes", {
      expect_equal(
        Sys.getenv("TABLEAU_API_TOKEN"),
        token
      )
      expect_equal(
        Sys.getenv("TABLEAU_SERVER_URL"),
        url
      )
      expect_equal(
        Sys.getenv("TABLEAU_API_VERSION"),
        version
      )
    })
  })
})
