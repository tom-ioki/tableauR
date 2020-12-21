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

describe("#sign_in", {
  name <- "My token"
  secret <- "ABCDEF"
  url <- "https://www.my-tableau-server.com"
  version <- 3.9

  example_url <-
    "https://www.my-tableau-server.com/api/3.9/auth/signin"

  request_body <-
    "<tsRequest>
    <credentials personalAccessTokenName=\"My token\" personalAccessTokenSecret=\"ABCDEF\">
      <site contentUrl=\"\"/>
    </credentials>
  </tsRequest>"
  example_body <-
    gsub(">[[:space:]]+", ">", request_body)

  describe("when the sign in was successful", {
    Sys.unsetenv("TABLEAU_API_VERSION")
    Sys.unsetenv("TABLEAU_SERVER_URL")
    Sys.unsetenv("TABLEAU_API_TOKEN")

    response <- list(
      status = "success",
      response = list(
        credentials = list(
          token = "FOOBAR"
        )
      )
    )

    get_access_token_stub <- stubthat::stub(get_access_token)
    get_access_token_stub$withArgs(tableau_server_url = example_url, request_body = example_body)$returns(response)
    check_for_api_error_stub <- stubthat::stub(check_for_api_error)
    check_for_api_error_stub$withArgs(api_response = response)$returns(response)

    sign_in_double <- function(name, secret, url, version) {
      mockr::with_mock(get_access_token = get_access_token_stub$f,
                       check_for_api_error = check_for_api_error_stub$f,
                       sign_in(token_name = name, token_secret = secret, server_url = url, api_version = version))
    }

    it("returns a confirmation message", {
      expect_message(
        sign_in_double(name = name, secret = secret, url = url, version = version),
        "Sign in was successful!"
      )
    })

    it("sets the environment variables", {
      expect_equal(
        Sys.getenv("TABLEAU_API_TOKEN"),
        "FOOBAR"
      )
      expect_equal(
        Sys.getenv("TABLEAU_SERVER_URL"),
        "https://www.my-tableau-server.com"
      )
      expect_equal(
        Sys.getenv("TABLEAU_API_VERSION"),
        "3.9"
      )
    })
  })

  describe("when the sign in failed", {
    Sys.unsetenv("TABLEAU_API_VERSION")
    Sys.unsetenv("TABLEAU_SERVER_URL")
    Sys.unsetenv("TABLEAU_API_TOKEN")

    response <-
      list(
        status = "error",
        error_code = "401001",
        error_summary = "Signin Error",
        error_detail = "An error occurred"
      )

    check_for_api_error_stub <- stubthat::stub(check_for_api_error)
    check_for_api_error_stub$withArgs(api_response = response)$returns(response)
    get_access_token_stub <- stubthat::stub(get_access_token)
    get_access_token_stub$withArgs(tableau_server_url = example_url, request_body = example_body)$returns(response)

    sign_in_double <- function(name, secret, url, version) {
      mockr::with_mock(get_access_token = get_access_token_stub$f,
                       check_for_api_error = check_for_api_error_stub$f,
                       sign_in(token_name = name, token_secret = secret, server_url = url, api_version = version))
    }

    it("returns an error message", {
      expect_error(
        sign_in_double(name = name, secret = secret, url = url, version = version)
      )
    })

    it("does not set the environment variables", {
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
})

describe("#create_sign_in_request_body", {
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
      create_sign_in_request_body(
        personal_access_token_name = pat_name,
        personal_access_token_secret = pat_secret,
        sign_in_to_site = sign_in_to_site
      ),
      expected_request_body
    )
  })
})
