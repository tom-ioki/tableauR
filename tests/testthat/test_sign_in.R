context("Sign in")

describe("Validation of function parameters", {
  name <- "My token"
  secret <- "ABCDEF"
  url <- "https://www.my-tableau-server.com"
  version <- 3.9

  expected_error_message <-
    "Missing authorization credentials: Make sure to provide all required credentials!"

  describe("when one of the function parameters is missing", {
    describe("token_name", {
      it("returns an error", {
        expect_error(
          sign_in(token_secret = secret, server_url = url, api_version = version),
          "argument \"token_name\" is missing, with no default"
        )
      })
    })

    describe("token_secret", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, server_url = url, api_version = version),
          "argument \"token_secret\" is missing, with no default"
        )
      })
    })

    describe("server_url", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, api_version = version),
          "argument \"server_url\" is missing, with no default"
        )
      })
    })

    describe("api_version", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, server_url = url),
          "argument \"api_version\" is missing, with no default"
        )
      })
    })
  })

  describe("when one of the function parameters is NULL", {
    describe("token_name", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = NULL, token_secret = secret, server_url = url, api_version = version),
          expected_error_message
        )
      })
    })

    describe("token_secret", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = NULL, server_url = url, api_version = version),
          expected_error_message
        )
      })
    })

    describe("server_url", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, server_url = NULL, api_version = version),
          expected_error_message
        )
      })
    })

    describe("site_url", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, server_url = url, site_url = NULL, api_version = version),
          expected_error_message
        )
      })
    })

    describe("api_version", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, server_url = url, api_version = NULL),
          expected_error_message
        )
      })
    })
  })

  describe("when one of the function parameters is NA", {
    describe("token_name", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = NA, token_secret = secret, server_url = url, api_version = version),
          expected_error_message
        )
      })
    })

    describe("token_secret", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = NA, server_url = url, api_version = version),
          expected_error_message
        )
      })
    })

    describe("server_url", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, server_url = NA, api_version = version),
          expected_error_message
        )
      })
    })

    describe("site_url", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, server_url = url, site_url = NA, api_version = version),
          expected_error_message
        )
      })
    })

    describe("api_version", {
      it("returns an error", {
        expect_error(
          sign_in(token_name = name, token_secret = secret, server_url = url, api_version = NA),
          expected_error_message
        )
      })
    })
  })

  describe("when the provided URL is not valid", {
    url <- "www.example.com"

    it("returns an error", {
      expect_error(
        sign_in(token_name = name, token_secret = secret, server_url = url, api_version = version),
        "Please provide a correct Tableau Server URL!"
      )
    })
  })
})

describe("#create_request_body", {
  pat_name <- "test_token"
  pat_secret <- "FOOBAR"
  sign_in_to_site <- "marketing"

  expected_request_body <-
    "<tsRequest>
      <credentials personalAccessTokenName=\"test_token\" personalAccessTokenSecret=\"FOOBAR\">
        <site contentUrl=\"marketing\"/>
      </credentials>
    </tsRequest>"
  expected_request_body <-
    gsub(">[[:space:]]+", ">", expected_request_body)

  it("returns the correct request body", {
    expect_equal(
      create_request_body(
        personal_access_token_name = pat_name,
        personal_access_token_secret = pat_secret,
        sign_in_to_site = sign_in_to_site
      ),
      expected_request_body
    )
  })
})
