context("HandleHTTPError")

test_that("Malformed arguments are handled", {
  expect_error(HandleHTTPError(), "SWS could not return reason for error")
  expect_error(HandleHTTPError(response = "moo"), "lexical error")
  expect_error(HandleHTTPError(status = 405), "SWS could not return reason for error")
})

test_that("Output is as it should be", {
  expect_error(HandleHTTPError(404, "{\"message\" : \"blarg\"}"), "Status code was 404.*Error message: blarg")
})
