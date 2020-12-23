context("Switch to site")

describe("Validation of function parameters", {
  url = "https://my-tableau-server.com"
  version = 3.9
  token = "FOOBAR"
  site = "marketing"

  expected_error_message <-
    "Missing parameters: Make sure to provide all required parameters!"

  error_not_signed_in <-
    "Not signed in to Tableau Server!"

  describe("when one of the function parameters is NULL", {
    it("returns an error", {
      expect_error(
        switch_to_site(server_url = NULL, api_version = version, api_token = token, site_url = site),
        expected_error_message
      )
      expect_error(
        switch_to_site(server_url = url, api_version = NULL, api_token = token, site_url = site),
        expected_error_message
      )
      expect_error(
        switch_to_site(server_url = url, api_version = version, api_token = NULL, site_url = site),
        expected_error_message
      )
      expect_error(
        switch_to_site(server_url = url, api_version = version, api_token = token, site_url = NULL),
        expected_error_message
      )
    })
  })

  describe("when one of the function parameters is NA", {
    it("returns an error", {
      expect_error(
        switch_to_site(server_url = NA, api_version = version, api_token = token, site_url = site),
        expected_error_message
      )
      expect_error(
        switch_to_site(server_url = url, api_version = NA, api_token = token, site_url = site),
        expected_error_message
      )
      expect_error(
        switch_to_site(server_url = url, api_version = version, api_token = NA, site_url = site),
        expected_error_message
      )
      expect_error(
        switch_to_site(server_url = url, api_version = version, api_token = token, site_url = NA),
        expected_error_message
      )
    })
  })

  describe("when one of the environment variables is blank", {
    it("returns an error", {
      expect_error(
        switch_to_site(server_url = "", api_version = version, api_token = token, site_url = site),
        error_not_signed_in
      )
      expect_error(
        switch_to_site(server_url = url, api_version = "", api_token = token, site_url = site),
        error_not_signed_in
      )
      expect_error(
        switch_to_site(server_url = url, api_version = version, api_token = "", site_url = site),
        error_not_signed_in
      )
    })
  })

  describe("when site_url is missing", {
    it("returns an error", {
      expect_error(
        switch_to_site(server_url = url, api_version = version, api_token = token),
        "argument \"site_url\" is missing, with no default"
      )
    })
  })
})

describe("#switch_to_site", {
  url <- "https://www.my-tableau-server.com"
  version <- "3.9"
  token <- "FOOBAR"
  site <- "marketing"

  Sys.setenv("TABLEAU_API_TOKEN" = token)
  Sys.setenv("TABLEAU_API_VERSION" = version)
  Sys.setenv("TABLEAU_SERVER_URL" = url)

  stubbed_url <-
    "https://www.my-tableau-server.com/api/3.9/auth/switchSite"

  stubbed_headers <-
    c('X-tableau-auth' = token)

  request_body <-
    "<tsRequest>
      <site contentUrl=\"marketing\"/>
    </tsRequest>"
  stubbed_request_body <-
    trim_xml(request_body)

  describe("when switching to another site succeeds", {
    response_body <-
      list(
        credentials = list(
          token = "FOOBAR",
          site = list(
            id = "12345a=12345b",
            contentUrl = "marketing"
          )
        )
      )

    response <-
      list(
        status = "success",
        response = response_body
      )

    post_switch_site_stub <- stubthat::stub(post_switch_site)
    post_switch_site_stub$withArgs(
      tableau_server_url = stubbed_url,
      request_body = stubbed_request_body,
      headers = stubbed_headers
    )$returns(response)

    check_for_api_error_stub <- stubthat::stub(check_for_api_error)
    check_for_api_error_stub$withArgs(api_response = response)$returns(response)

    switch_to_site_double <- function(url, token, version, site) {
      mockr::with_mock(
        post_switch_site = post_switch_site_stub$f,
        check_for_api_error = check_for_api_error_stub$f,
        switch_to_site(
          server_url = url,
          api_token = token,
          api_version = version,
          site = site
        )
      )
    }

    it("returns a confirmation message", {
      expect_message(
        switch_to_site_double(url = url, token = token, version = version, site = site),
        "Successfully switched to site: marketing"
      )
    })
  })

  describe("when switching to another site fails", {
    response <-
      list(
        status = "error",
        error_code = "401003",
        error_summary = "Switch site Error",
        error_detail = "There was a problem switching sites. The site might be unavailable or is not found."
      )

    post_switch_site_stub <- stubthat::stub(post_switch_site)
    post_switch_site_stub$withArgs(
      tableau_server_url = stubbed_url,
      request_body = stubbed_request_body,
      headers = stubbed_headers
    )$returns(response)

    check_for_api_error_stub <- stubthat::stub(check_for_api_error)
    check_for_api_error_stub$withArgs(api_response = response)$returns(response)

    switch_to_site_double <- function(url, token, version, site) {
      mockr::with_mock(
        post_switch_site = post_switch_site_stub$f,
        check_for_api_error = check_for_api_error_stub$f,
        switch_to_site(
          server_url = url,
          api_token = token,
          api_version = version,
          site = site
        )
      )
    }

    it("returns an error", {
      expect_error(
        switch_to_site_double(url = url, token = token, version = version, site = site)
      )
    })
  })
})
