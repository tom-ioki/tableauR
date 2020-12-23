context("Utils")

describe("#compose_server_url", {
  api_version <- 3.9
  endpoint <- 'test_endpoint'
  expected_url <-
    paste0("https://www.example-url.com/api/", api_version, endpoint)

  describe("when the provided url ends with '/'", {
    url <- "https://www.example-url.com/"

    it("it returns the correct composed request url", {
      expect_equal(
        compose_server_url(url = url, version = api_version, end_point = endpoint),
        expected_url
      )
    })
  })

  describe("when the provided url ends without '/'", {
    url <- "https://www.example-url.com"

    it("it returns the correct composed request url", {
      expect_equal(
        compose_server_url(url = url, version = api_version, end_point = endpoint),
        expected_url
      )
    })
  })
})

describe("#trim_xml", {
  raw_xml <-
    "<tsRequest>
      <site contentUrl=\"marketing\"/>
      <random foo=\"bar\"/>
    </tsRequest>"

  formatted_xml <-
    "<tsRequest><site contentUrl=\"marketing\"/><random foo=\"bar\"/></tsRequest>"

  it("returns the correctly formatted xml", {
    expect_equal(
      trim_xml(raw_xml),
      formatted_xml
    )
  })
})
