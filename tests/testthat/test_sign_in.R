context("Sign in")

name <- "My token"
secret <- "ABCDEF"
url <- "https://www.my-tableau-server.com"
version <- 3.9

expected_error_message <-
  "Missing authorization credentials: Make sure to provide all required credentials!"

describe("when one of the function parameters is missing", {
  expected_error_message <-
    "Missing authorization credentials: Make sure to provide all required credentials!"

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
