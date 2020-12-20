context("Validations")

describe("#is_url", {
  it("returns an error when input is not a url", {
    expect_error(
      is_url("FOOBAR"),
      "Please provide a correct Tableau Server URL!"
    )
  })

  it("does nothing when the input is a correct url", {
    expect_null(
      is_url("https://cran.r-project.org/")
    )
  })
})

describe("#is_null", {
  parameters <-
    list(a = 1, b = 2)
  parameters_with_null <-
    list(a = NULL, b = 2)
  it("detects a NULL parameter", {
    expect_false(
      is_null(parameters)
    )

    expect_true(
      is_null(parameters_with_null)
    )
  })
})

describe("#is_na", {
  parameters <-
    list(a = 1, b = 2)
  parameters_with_na <-
    list(a = NA, b = 2)
  it("detects a NA parameter", {
    expect_false(
      is_na(parameters)
    )

    expect_true(
      is_na(parameters_with_na)
    )
  })
})

describe("is_blank", {
  parameters<-
    list(a = 1, b = 2)
  parameters_with_blank <-
    list(a = "", b = 2)
  it("detects blank parameters", {
    expect_false(
      is_blank(parameters)
    )

    expect_true(
      is_blank(parameters_with_blank)
    )
  })
})
